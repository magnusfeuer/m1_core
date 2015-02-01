/*
 * EPIC "clip" region implementation
 *
 *   The code store start point and length and edge alpha values
 *
 */


typedef struct _eclip_segment {
    int       x0;   // start point 
    u_int16_t len;  // length
    u_int8_t  a0;   // alpha at start point
    u_int8_t  an;   // alpha at end point
} ERegionSegment;

typedef struct _eclip_row {
    u_int16_t nsegs;   // number of consecutive segments
    u_int8_t  a;       // over all alpha value (all pixels in row)
    u_int8_t  flags;   // flags...
    ERegionSegment* seg; // segments
} ERegionRow;

typedef struct _eregion {
    int y0;       // clip y start
    int y1;       // clip y stop
    ERRow* row;   // indexed from y0 (0) .. y1 (y1-y0+1)
    int nused;    // number of used segments
    int nsegs;    // number of allocated segments
    ERegionSegement segments[0];
} ERegion;

bool PointInRegion(ERegion* r, EPoint_t* p)
{
    ERegionRow* row;
    int i;

    if (p->y < r->xy.y0) return false;
    if (p->y > r->xy.y1) return false;

    row = &r->rows[p->y - r->xy.y0];
    for (i = 0; i < row->n; i++) {
	if (r.xy.x < row->seg[i].x0) return false;
	if (r.xy.x < row->seg[i].x0 + row->seg[i].len) return true;
    }
    return false;
}

ERegion* ERegionCreate(void)
{
    size_t sz = sizeof(ERegion);
    ERegion* r = (ERegion*) malloc(sz);
    r->y0 = 0;
    r->y1 = 0;
    r->row = NULL;
    return r;
}

ERegion* ERegionCreateArea(ERect_t* rect)
{
    size_t sz = sizeof(ERegion)+sizeof(ERegionSegment);
    ERegion* r = (ERegion*) malloc(sz);
    int i;
    int w = rect->wh.width;
    int h = rect->wh.height;

    r->row = malloc(h*sizeof(ERRow));
    r->y0  = rect->xy.y;
    r->y1  = rect->xy.y + h - 1;
    r->nsegs = 1;
    r->segment[0].x0  = rect.xy.x;
    r->segment[0].len = w;
    r->segment[0].a0 = 255;
    r->segment[0].a1 = 255;

    i = 0;
    while(h--) {
	r->row[i].n = rect->wh.w;
	r->row[i].a = 255;
	r->row[i].flags = 0;
	r->row[i].seg = &r->segments[0];
	i++;
    }
}

/* Form union of two EPic regions */
ERegion* ERegionUnion(ERegion* a, ERegion* b)
{
    int y0 = EMinInt(a->y0, b->y0);
    int y1 = EMaxInt(a->y1, b->y1);
    int nsegs = a->nsegs + b->nsegs;
    size_t sz = sizeof(ERegion)+nsegs*sizeof(ERegionSegment);
    ERegion* r;
    int i = 0;

    r = (ERegion*) malloc(sz);
    for (i = 0; i < a->nsegs; i++)  r->segments[i] = a->segments[i];
    for (i = 0; i < b->nsegs; i++)  r->segments[i+a->nsegs] = b->segments[i];
    
    r->row = malloc(((y1-y0)+1)*sizeof(ERegionRow));

    if (a->y0 < b->y0) {
	while(ay0 < by0) {
	    r->row[i] = 
	    
	
    }
    else if (b->y0 < a->y0) {
	/* copy rows from b */
    }
	
    

}

/* Form intersection of two EPic regions */
ERegion* ERegionIntersect(ERegion* a, ERegion* b)
{

}
