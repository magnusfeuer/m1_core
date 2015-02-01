/*
** Linear hash of Epic objects
*/
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

#include "epic.h"

#define EHASH_SZEXP   8
#define EHASH_SEGSZ   (1 << EHASH_SZEXP)
#define EHASH_SZMASK  ((1 << EHASH_SZEXP)-1)

#define EHASH_SEG(i)  ((i)>>EHASH_SZEXP)
#define EHASH_POS(i)  ((i)&EHASH_SZMASK)

#define EHASH_SEG_LEN         256   /* When growing init segs */
#define EHASH_SEG_INCREAMENT  128   /* Number of segments to grow */

#define EHASH_BUCKET(lh, i) (lh)->seg[EHASH_SEG(i)][EHASH_POS(i)]

#define EHASH_IX(lh, hval) \
    (((((hval) & (lh)->szm)) < (lh)->p) ? \
       ((hval) & (((lh)->szm << 1) | 1)) : \
       (((hval) & (lh)->szm)))
      

static EBucket** EHash_alloc_seg(int seg_sz)
{
    EBucket** bp;
    int sz = sizeof(EBucket*)*seg_sz;

    bp = (EBucket**) malloc(sz);
    memset(bp, 0, sz);
    return bp;
}

inline static EBucket** EHash_HLOOKUP(EHash* lh,
					   hash_value_t hval,
					   void* key)
{
    int ix = EHASH_IX(lh, hval);
    EBucket** bpp = &EHASH_BUCKET(lh, ix);
    EBucket* b = *bpp;

    while(b != (EBucket*) 0) {
	if ((b->hvalue == hval) && (lh->cmp(key, (void*) b) == 0))
	    return bpp;
	bpp = &b->next;
	b = b->next;
    }
    return bpp;
}

/* scan bucket for key return bucket */
inline static EBucket** EHash_LOOKUP(EHash* lh, void* key)
{
    return EHash_HLOOKUP(lh, lh->hash(key), key);
}


EHash* EHashInit(EHash* lh, char* name, int thres,
		 hash_value_t (*hash)(void*),
		 int (*cmp)(void*, void*),
		 void (*release)(void*),
		 void* (*copy)(void*))
{
    EBucket*** bp;

    if ((bp = (EBucket***) malloc(sizeof(EBucket**))) == NULL)
	return NULL;
    lh->hash    = hash;
    lh->cmp     = cmp;
    lh->release = release;
    lh->copy    = copy;
    lh->is_allocated = 0;
    lh->name = name;
    lh->thres = thres;
    lh->szm = EHASH_SZMASK;
    lh->nactive = EHASH_SEGSZ;
    lh->nitems = 0;
    lh->p = 0;
    lh->nsegs = 1;
    lh->seg = bp;
    lh->seg[0] = EHash_alloc_seg(EHASH_SEGSZ);
    lh->nslots = EHASH_SEGSZ;
    lh->n_seg_alloc = 1;
    lh->n_seg_free  = 0;
    lh->n_resize    = 0;
    return lh;
}


static void EHashGrow(EHash* lh)
{
    EBucket** bp;
    EBucket** bps;
    EBucket* b;
    unsigned int ix;
    unsigned int nszm = (lh->szm << 1) | 1;

    EDBGFMT("EHash: grow");

    if (lh->nactive >= lh->nslots) {
	EDBGFMT("EHash: grow, adding %d new slots", EHASH_SEGSZ);
	/* Time to get a new array */
	if (EHASH_POS(lh->nactive) == 0) {
	    unsigned int six = EHASH_SEG(lh->nactive);
	    if (six == lh->nsegs) {
		int i, sz;

		if (lh->nsegs == 1)
		    sz = EHASH_SEG_LEN;
		else
		    sz = lh->nsegs + EHASH_SEG_INCREAMENT;
		lh->seg = (EBucket***) realloc(lh->seg,
						      sizeof(EBucket**)*sz);
		lh->nsegs = sz;
		lh->n_resize++;
		for (i = six+1; i < sz; i++)
		    lh->seg[i] = 0;
	    }
	    lh->seg[six] = EHash_alloc_seg(EHASH_SEGSZ);
	    lh->nslots += EHASH_SEGSZ;
	    lh->n_seg_alloc++;
	}
    }

    ix = lh->p;
    bp = &EHASH_BUCKET(lh, ix);
    ix += (lh->szm+1);
    bps = &EHASH_BUCKET(lh, ix);
    b = *bp;

    while (b != 0) {
	ix = b->hvalue & nszm;

	if (ix == lh->p)
	    bp = &b->next;          /* object stay */
	else {
	    *bp = b->next;  	    /* unlink */
	    b->next = *bps;         /* link */
	    *bps = b;
	}
	b = *bp;
    }

    lh->nactive++;
    if (lh->p == lh->szm) {
	lh->p = 0;
	lh->szm = nszm;
    }
    else
	lh->p++;
}

/*
** Shrink the hash table
** Remove segments if they are empty
** but do not reallocate the segment index table !!!
*/
static void EHashShrink(EHash* lh)
{
    EBucket** bp;

    EDBGFMT("EHash: shrink");

    if (lh->nactive == EHASH_SEGSZ)
	return;

    lh->nactive--;
    if (lh->p == 0) {
	lh->szm >>= 1;
	lh->p = lh->szm;
    }
    else
	lh->p--;

    bp = &EHASH_BUCKET(lh, lh->p);
    while(*bp != 0) 
	bp = &(*bp)->next;

    *bp = EHASH_BUCKET(lh, lh->nactive);
    EHASH_BUCKET(lh, lh->nactive) = 0;

    if ((lh->nactive & EHASH_SZMASK) == EHASH_SZMASK) {
	int six = EHASH_SEG(lh->nactive)+1;

	EDBGFMT("EHash: shrink, removing %d slots", EHASH_SEGSZ);
	free(lh->seg[six]);
	lh->seg[six] = NULL;
	lh->nslots -= EHASH_SEGSZ;
	lh->n_seg_free++;
    }
}

EHash* EHashNew(char* name, int thres,
		hash_value_t (*hash)(void*),
		int (*cmp)(void*, void*),
		void (*release)(void*),
		void* (*copy)(void*))
{
    EHash* tp;

    if ((tp = (EHash*) malloc(sizeof(EHash))) == NULL)
	return NULL;
    
    if (EHashInit(tp, name, thres, hash, cmp, release, copy) == NULL) {
	free(tp);
	return NULL;
    }
    tp->is_allocated = 1;
    return tp;
}


void EHashDelete(EHash* lh)
{
    EBucket*** sp = lh->seg;
    int n = lh->nsegs;

    while(n--) {
	EBucket** bp = *sp;
	if (bp != 0) {
	    int m = EHASH_SEGSZ;
	    while(m--) {
		EBucket* p = *bp++;
		while(p != 0) {
		    EBucket* next = p->next;
		    if (lh->release)
			(*lh->release)((void*) p);
		    p = next;
		}
	    }
	    free(*sp);
	}
	sp++;
    }
    free(lh->seg);

    if (lh->is_allocated)
	free(lh);
}

void* EHashInsertNew(EHash* lh, void* key, void* data)
{
    hash_value_t hval = lh->hash(key);
    EBucket** bpp = EHash_HLOOKUP(lh, hval, key);
    EBucket* b = *bpp;

    if (b != NULL) {
	EDBGFMT("EHash: insert_new exists");
	/* release data if copy function is not defined */
	if (lh->copy==NULL) {
	    if (lh->release) lh->release(data);
	}
	return NULL;
    }
    EDBGFMT("EHash: insert_new");
    b = (EBucket*) ((lh->copy != NULL) ? lh->copy(data) : data);
    b->hvalue = hval;
    b->next = *bpp;
    *bpp = b;
    lh->nitems++;

    if ((lh->nitems / lh->nactive) >= lh->thres)
	EHashGrow(lh);
    return (void*) b;
}

void* EHashInsert(EHash* lh, void* key, void* data)
{
    hash_value_t hval = lh->hash(key);
    EBucket** bpp = EHash_HLOOKUP(lh, hval, key);
    EBucket* b = *bpp;

    EDBGFMT("EHash: insert");

    if (b != NULL) {
	EBucket* b_next = b->next;
	if (lh->release != NULL) lh->release(b);
	/* printf("REPLACE %s\n", (char*) key); */
	b = (EBucket*) ((lh->copy != NULL) ? lh->copy(data) : data);
	b->hvalue = hval;
	b->next = b_next;
	*bpp = b;
    }
    else {
	/* printf("INSERT %s\n", (char*) key); */
	b = (EBucket*) ((lh->copy != NULL) ? lh->copy(data) : data);
	b->hvalue = hval;
	b->next   = NULL;
	*bpp = b;
	lh->nitems++;

	if ((lh->nitems / lh->nactive) >= lh->thres)
	    EHashGrow(lh);
    }
    return (void*) b;

}


void* EHashLookup(EHash* lh, void* key)
{
    EBucket** bpp = EHash_LOOKUP(lh, key);

    EDBGFMT("EHash: lookup %p", key);

    return *bpp;
}

/*
** Erase an item
*/
void* EHashErase(EHash* lh, void* key)
{
    EBucket** bpp = EHash_LOOKUP(lh, key);
    EBucket* b = *bpp;

    /* printf("ERASE %s\n", (char*) key); */

    EDBGFMT("EHash: erase");

    if (b != NULL) {
	*bpp = b->next;  /* unlink */
	if (lh->release) lh->release((void*) b);
	lh->nitems--;
	if ((lh->nitems / lh->nactive) < lh->thres)
	    EHashShrink(lh);
    }
    return (void*)b;
}

void EHashEach(EHash* lh, void (elem)(EHash* lh, void* elem, void* arg),
	       void* arg)
{
    int i;
    int nslots = lh->nslots;

    for (i = 0; i < nslots; i++) {
	EBucket* list = EHASH_BUCKET(lh, i);
	while(list) {
	    EBucket* next = list->next;
	    elem(lh, (void*) list, arg);
	    list = next;
	}
    }
}


void EHashInfo(EHash* lh)
{
    unsigned int i;
    int depth = 0;

    for (i = 0; i < lh->nslots; i++) {
	EBucket* list = EHASH_BUCKET(lh, i);
	int d = 0;

	while(list) {
 	    list = list->next;
	    d++;
	}
	if (d > depth)
	    depth = d;
    }
    printf("  Name: %s\n", lh->name);
    printf("  Size: %d\n", lh->szm+1);
    printf("Active: %d\n", lh->nactive);    
    printf(" Split: %d\n", lh->p);
    printf(" Items: %d\n", lh->nitems);
    printf(" Slots: %d\n", lh->nslots);
    printf("  Segs: %d\n", lh->nsegs);
    printf(" Thres: %d\n", lh->thres);
    printf(" Ratio: %e\n", (float) lh->nitems / (float) lh->nactive);
    printf("   Max: %d\n", depth);
    printf("Resize: %d\n", lh->n_resize);
    printf(" Alloc: %d\n", lh->n_seg_alloc);
    printf("  Free: %d\n", lh->n_seg_free);
}
