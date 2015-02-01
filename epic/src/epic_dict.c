/*
 * EPIC Dictionary
 *
 *   Used to store key value pairs:
 * Special feature:
 *   overlay representation: a key may have data on several data formats
 *      "foo"   12
 *      "foo"   12.0
 *      "foo"   "12"
 *   This is implemented by using data_type when lookup key
 *       so key_type, data_type and key must match.
 */

#include "epic.h"

#define EDICT_ENTRY_CHUNK 32

static int edict_bcmp(const void* k, const void* e)
{
    EDictData* key = (EDictData*) k;
    EDictEntry* ent = *((EDictEntry**)e);
    if (key->len != ent->key.len)
	return key->len - ent->key.len;
    if (key->type != ent->key.type)
	return (int) key->type - (int) ent->key.type;
    return memcmp(key->ptr, ent->key.ptr, key->len);
}

static int edict_cmp(const void* a0, const void* b0)
{
    EDictEntry* a = *((EDictEntry**)a0);
    EDictEntry* b = *((EDictEntry**)b0);

    if (a->key.len != b->key.len)
	return a->key.len - b->key.len;
    if (a->key.type != b->key.type)
	return (int) a->key.type - (int) b->key.type;
    return memcmp(a->key.ptr, b->key.ptr, a->key.len);
}

void EDictInit(EDict* dict)
{
    EOBJECT_INIT(dict, EDICT_TYPE);
    dict->entries    = 0;
    dict->used       = 0;
    dict->is_sorted  = 1;
    dict->entry      = NULL;
}

EDict* EDictCreate()
{
    EDict* dict;

    if ((dict = (EDict*) malloc(sizeof(EDict))) == NULL)
	return NULL;
    EDictInit(dict);
    dict->on_heap = 1;
    dict->refc = 1;
    return dict;
}

void EDICT_TYPE_RELEASE(void* arg)
{
    EDict* dict = (EDict*) arg;

    if (!dict) {
	EDBGFMT("EDICT_TYPE_RELEASE: NULL POINTER: %p", arg);
	return;
    }
    
    EDBGFMT("EDICT_TYPE_RELEASE: %p", arg);
    if (dict->entry != NULL)
	free(dict->entry);
    if (dict->on_heap)
	free(dict);
}

void EDictSort(EDict* dict)
{
    if (!dict) 
	return;

    if (!dict->is_sorted) {
	qsort(dict->entry, dict->used, sizeof(EDictEntry**), edict_cmp);
	dict->is_sorted = 1;
    }
}
	

static void edict_update_ent(EDictEntry* ent, EDictData* key, EDictData* data)
{
    u_int8_t* ptr = ent->mem;

    if (!ent)
	return;

    if (key->type == EDICT_INTEGER)
	ptr = (u_int8_t*) EPIC_ALIGN(ptr, sizeof(int));
    else if (key->type == EDICT_FLOAT)
	ptr = (u_int8_t*) EPIC_ALIGN(ptr, sizeof(double));	
    ent->key.ptr = (void*) ptr;
    ent->key.len = key->len;
    ent->key.type = key->type;
    memcpy(ent->key.ptr, key->ptr, key->len);

    ptr += key->len;
    if (data->type == EDICT_INTEGER)
	ptr = (u_int8_t*) EPIC_ALIGN(ptr, sizeof(int));
    else if (data->type == EDICT_FLOAT)
	ptr = (u_int8_t*) EPIC_ALIGN(ptr, sizeof(double));	
    ent->data.ptr = (void*) ptr;
    ent->data.len = data->len;
    ent->data.type = data->type;
    memcpy(ent->data.ptr, data->ptr, data->len);
}


static int edict_add_ent(EDict* dict, EDictEntry* ent)
{
    size_t sz;

    if (!dict) 
	return -1;

    if (dict->used < dict->entries) {
	dict->entry[dict->used++] = ent;
	dict->is_sorted = 0;
    }
    else if (dict->entry == NULL) {
	sz = sizeof(EDictEntry*)*EDICT_ENTRY_CHUNK;
	if ((dict->entry = (EDictEntry**) malloc(sz)) == NULL) {
	    free(ent);
	    return -1;
	}
	dict->entry[0] = ent;
	dict->entries = EDICT_ENTRY_CHUNK;
	dict->used    = 1;
	dict->is_sorted  = 1;
    }
    else {
	EDictEntry** entry;
	sz = sizeof(EDictEntry*)*(dict->entries+EDICT_ENTRY_CHUNK);
	if ((entry = (EDictEntry**) realloc(dict->entry,sz)) == NULL) {
	    free(ent);
	    return -1;
	}
	dict->entry = entry;
	dict->entry[dict->used++] = ent;
	dict->entries += EDICT_ENTRY_CHUNK;
	//	dict->used    += 1; // Magnus 2008-02-07. We did increase dict->used above.
	dict->is_sorted  = 0;
    }
    return 0;
}


int EDictLookupIx(EDict* dict, EDictData* key, EDictType data_type)
{
    if (!dict)
	return -1;

    if (dict->is_sorted) {
	EDictEntry** ep = (EDictEntry**) 
	    bsearch(key, dict->entry, dict->used, 
		    sizeof(EDictEntry*), edict_bcmp);
	if (ep == NULL)
	    return -1;
	return ep - dict->entry;
    }
    else {
	int i;
	for (i = 0; i < (int) dict->used; i++) {
	    if ((dict->entry[i]->key.len == key->len) &&
		(dict->entry[i]->key.type == key->type) &&
		((data_type == EDICT_ANY) ||
		 (dict->entry[i]->data.type == data_type)) &&
		(memcmp(dict->entry[i]->key.ptr, key->ptr, key->len) == 0))
		return i;
	}
	return -1;
    }
}

/* remove index ix and compact */
int EDictDelIx(EDict* dict, int ix)
{
    if (!dict)
	return -1;

    if ((ix < 0) || (ix >= (int) dict->used))
	return -1;
    free(dict->entry[ix]);
    dict->entry[ix] = NULL;
    dict->used--;
    if (ix < (int) dict->used) {
	if (dict->is_sorted) {
	    /* keep sorted */
	    unsigned int sz = (dict->used - ix)*sizeof(EDictEntry*);
	    memmove(&dict->entry[ix], &dict->entry[ix], sz);
	}
	else {
	    /* just swap */
	    dict->entry[ix] = dict->entry[dict->used];
	    dict->entry[dict->used] = NULL;
	}
    }
    return 0;
}

EDictEntry* EDictLookupEnt(EDict* dict,EDictData* key, EDictType data_type)
{
    if (!dict)
	return NULL;

    int ix;
    if ((ix = EDictLookupIx(dict, key, data_type)) >= 0)
	return dict->entry[ix];
    return NULL;
}


int EDictSetEnt(EDict* dict, EDictData* key, EDictData* data)
{
    size_t size = sizeof(EDictEntry);
    size_t mem_size = 0;
    int ix;

    if (!dict)
	return -1;

    /* calculate alignable memory size */
    if (key->type == EDICT_INTEGER)
	mem_size += (key->len + (sizeof(int)-1));
    else if (key->type == EDICT_FLOAT)
	mem_size += (key->len + (sizeof(double)-1));
    else
	mem_size += key->len;

    if (data->type == EDICT_INTEGER)
	mem_size += (data->len + (sizeof(int)-1));
    else if (data->type == EDICT_FLOAT)
	mem_size += (data->len + (sizeof(double)-1));
    else
	mem_size += data->len;

    if ((ix = EDictLookupIx(dict, key, data->type)) >= 0) {
	/* update entry */
	EDictEntry* ent = dict->entry[ix];
	u_int32_t esize = sizeof(EDictEntry) + ent->mem_size;
	if (esize >= size) {
	    /* just overwrite the value & type (FIXME: realign) */
	    ent->data.type = data->type;
	    memcpy(ent->data.ptr, data->ptr, data->len);
	}
	else {
	    /* reallocate */
	    if ((ent = (EDictEntry*) realloc(ent, size+mem_size)) == NULL)
		return -1;
	    ent->mem_size = mem_size;
	    dict->entry[ix] = ent;
	    edict_update_ent(ent, key, data);
	}
	return 0;
    }
    else {
	EDictEntry* ent;

	if ((ent = (EDictEntry*) malloc(size+mem_size)) == NULL)
	    return -1;
	ent->mem_size = mem_size;
	edict_update_ent(ent, key, data);
	return edict_add_ent(dict, ent);
    }
}

int EDictUnsetEnt(EDict* dict, EDictData* key, EDictType data_type)
{
    int ix;

    if (!dict)
	return -1;

    while ((ix = EDictLookupIx(dict, key, data_type))>=0) {
	EDictDelIx(dict, ix);
	if (data_type != EDICT_ANY)
	    return 0;
    }
    return 0;
}

int EDictUnset(EDict* dict, char* key)
{
    EDictData ekey = { EDICT_STRING, strlen(key)+1, key };

    return EDictUnsetEnt(dict, &ekey, EDICT_ANY);
}

int EDictLookupString(EDict* dict, char* key, char** value)
{
    EDictData ekey = { EDICT_STRING, strlen(key)+1, key };
    int ix;
    if ((ix = EDictLookupIx(dict, &ekey, EDICT_STRING)) < 0)
	return -1;
    *value = (char*) dict->entry[ix]->data.ptr;
    return 0;
}

int EDictLookupInteger(EDict* dict, char* key, int* value)
{
    EDictData ekey = { EDICT_STRING, strlen(key)+1, key };
    int ix;
    if ((ix = EDictLookupIx(dict, &ekey, EDICT_INTEGER)) < 0)
	return -1;
    *value = *((int*) dict->entry[ix]->data.ptr);
    return 0;
}

int EDictLookupFloat(EDict* dict, char* key, double* value)
{
    EDictData ekey = { EDICT_STRING, strlen(key)+1, key };
    int ix;
    if ((ix = EDictLookupIx(dict, &ekey, EDICT_FLOAT)) < 0)
	return -1;
    *value = *((double*) dict->entry[ix]->data.ptr);
    return 0;
}

int EDictLookupBinary(EDict* dict, char* key, void** value, size_t* len)
{
    EDictData ekey = { EDICT_STRING, strlen(key)+1, key };
    int ix;
    if ((ix = EDictLookupIx(dict, &ekey, EDICT_BINARY)) < 0)
	return -1;
    *value = (void*) dict->entry[ix]->data.ptr;
    *len   = dict->entry[ix]->data.len;
    return 0;
}

int EDictSetString(EDict* dict, char* key, char* value)
{
    EDictData ekey  = { EDICT_STRING, strlen(key)+1, key };
    EDictData edata = { EDICT_STRING, strlen(value)+1, value };
    return EDictSetEnt(dict, &ekey, &edata);
}

int EDictSetBinary(EDict* dict, char* key, void* value, size_t len)
{
    EDictData ekey  = { EDICT_STRING, strlen(key)+1, key };
    EDictData edata = { EDICT_BINARY, len, value };
    return EDictSetEnt(dict, &ekey, &edata);
}

int EDictSetInteger(EDict* dict, char* key, int value)
{
    EDictData ekey  = { EDICT_STRING, strlen(key)+1, key };
    EDictData edata = { EDICT_INTEGER, sizeof(value), &value};
    return EDictSetEnt(dict, &ekey, &edata);
}

int EDictSetFloat(EDict* dict, char* key, double value)
{
    EDictData ekey  = { EDICT_STRING, strlen(key)+1, key };
    EDictData edata = { EDICT_FLOAT, sizeof(value), &value};
    return EDictSetEnt(dict, &ekey, &edata);
}
