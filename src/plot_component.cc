//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#include "plot_component.hh"
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <math.h>
#include <float.h>

XOBJECT_TYPE_BOOTSTRAP(CPlotComponent);

CPlotComponent::CPlotComponent(CExecutor* aExec, CBaseType *aType) :
    CLayerComponent(aExec, aType),
    mPlotType(this),
    mViewType(this),
    mPlotTime(this),
    mColor(this),
    mBorderWidth(this),
    mBorderColor(this)
{
    mPlotType.putValue(aExec, PLOT_DOT);
    mViewType.putValue(aExec, VIEW_SAMPLES);
    mPlotTime.putValue(aExec, 1.0);
    mColor.putValue(aExec, 0xff0000);
    mBorderWidth.putValue(aExec, 0);
    mBorderColor.putValue(aExec, 0x000000);
    mFft         = NULL;
    mYt          = NULL;
    mYmin        = -1;
    mYmax        = 1;
    mFftCount    = 0;

    eventPut(aExec, XINDEX(CPlotComponent,plotType),    &mPlotType);
    eventPut(aExec, XINDEX(CPlotComponent,viewType),    &mViewType);
    eventPut(aExec, XINDEX(CPlotComponent,plotTime),    &mPlotTime);
    eventPut(aExec, XINDEX(CPlotComponent,color),       &mColor);
    eventPut(aExec, XINDEX(CPlotComponent,borderWidth), &mBorderWidth);
    eventPut(aExec, XINDEX(CPlotComponent,borderColor), &mBorderColor);
}

CPlotComponent::~CPlotComponent(void)
{
    if (mFft) delete mFft;
    if (mYt)  delete mYt;
}

void CPlotComponent::execute(CExecutor* aExec)
{

}

//!
//! Resample the samples i aSamples buffer so that they spread
//! at "fixed" times upto xn number of samples and store the
//! normalized samples in x.
//! Also return the min, max values and the start and stop time
//! for the samples returned.
//!
bool CPlotComponent::resample(CSamplerBase* aSamples, float* x, size_t xn,
			      float &x_min, float &x_max, 
			      TimeStamp &t_min, TimeStamp &t_max, 
			      double &rAvgSampleTime)
{
    int    max_n = aSamples->sampleCount();
    int    active_n;
    float  max_v = aSamples->maxValue();
    float  min_v = aSamples->minValue();
    int    ix_begin;
    int    ix_end;
    TimeStamp  t_begin;
    TimeStamp  t_end;
    float      t_step;

    ix_begin  = aSamples->index();
    // Skip zero time samples... (normal in init)
    active_n = max_n;
    while((aSamples->sample(ix_begin)->ts() == 0) && active_n) {
	ix_begin = (ix_begin + 1) % max_n;
	active_n--;
    }

    if (active_n < (int)xn/4)  // at least 25% of sample needed ... 
	return false;

    ix_end = (ix_begin + active_n - 1) % max_n;  // last sample

    t_begin = t_min = aSamples->sample(ix_begin)->ts();
    t_end   = t_max = aSamples->sample(ix_end)->ts();

    //printf("IX_BEGIN=%d, IX_END=%d, ACTIVE=%d\n", ix_begin, ix_end, active_n);

    {
	TimeStamp t0    = t_begin;
	TimeStamp t_sum = 0;
	int n           = active_n-1;
	int ix          = ix_begin;

	while(n--) {
	    TimeStamp t;
	    ix = (ix+1) % max_n;
	    t  = aSamples->sample(ix)->ts();
	    t_sum += (t - t0);
	    t0 = t;
	}
	rAvgSampleTime = (t_sum/double(STAMP_SEC)) / active_n;
    }
	
    t_step = float(t_end - t_begin) / xn;
    // printf("T_STEP = %f\n", t_step);
    {
	TimeStamp t0 = aSamples->sample(ix_begin)->ts();
	float v0 = aSamples->sample(ix_begin)->val();
	float t;
	float td;
	TimeStamp t1;
	float v1;
	int   i;
	int   ix;

	v0 = (v0 > max_v) ? max_v : ((v0 < min_v) ? min_v : v0);
	ix    = (ix_begin+1) % max_n;
	t1    = aSamples->sample(ix)->ts();
	v1    = aSamples->sample(ix)->val();
	v1    = (v1 > max_v) ? max_v : ((v1 < min_v) ? min_v : v1);
	x[0]  = v0;
	x_min = x_max = v0;
	i     = 1;
	t     = t0;
	td    = t1-t0;
	while((t < t_end) && (i < (int)xn)) {
	    float v;
	    t += t_step;
	    while ((t >= t1) && (t1 < t_end)) {
		t0    = t1;
		v0    = v1;
		ix    = (ix+1) % max_n;
		t1    = aSamples->sample(ix)->ts();
		td    = t1-t0;
		v1    = aSamples->sample(ix)->val();
		v1    = (v1 > max_v) ? max_v : ((v1 < min_v) ? min_v : v1);
	    }
	    v = v0 + (v1-v0)*((t-t0)/td);
	    if (v < x_min) x_min = v;
	    if (v > x_max) x_max = v;
	    x[i++] = v;
	}
    }
    return true;
}

//
// Find N greates values among samples in x and store them in the maxy
// result array. if iy != NULL then also store the index.
//
int CPlotComponent::maxValues(float* y, size_t yn, float* maxy, int* iy, int ym)
{
    int i;

    for (i = 0; i < ym; i++)
	maxy[i] = FLT_MIN;
    for (; i < (int) yn; i++) {
	float v = y[i];
	int j   = 0;
	while((j < (int) ym) && (v < maxy[j]))
	    j++;
	if (j < (int) ym) {
	    maxy[j] = v;
	    if (iy) iy[j] = i;
	}
    }
    return ym;
}

//
// draw frequency present a fourier transform display
// plotting the frequency magnitude given a (resampled)
// data buffer from Sampler over the plotTime
//
void CPlotComponent::draw_frequency(CRedrawContext *aContext,
				    CSamplerBase*   aSamples)
{
    float xt[1024];
    int   y0;
    int   x;
    int   x0 = int(aContext->lLeft);
    int   x1 = int(aContext->lLeft + aContext->cWidth - 1);
    int   i;
    float xt_min, xt_max;
    float y_mid, y_range;
    TimeStamp t_min, t_max;
    bool  isPeak[1024];

    mFftCount++;
    if (mFftCount % 30 != 0) { // aprox once a second (with fps=30)
	if (mFftCount == 1)
	    return;
	if (mYt)
	    goto display_old;
	return;
    }

    if (!resample(aSamples, xt, 1024, xt_min, xt_max, t_min, t_max, mAvgSampleTime)) {
	// fprintf(stderr, "CPlotComponent: to few samples\n");
	return;
    }
    printf("RESAMPLE: xt_min=%.2f, xt_max=%.2f, t_min=%.2f, t_max=%.2f, T=%.8f F=%.2f\n", 
	   xt_min, xt_max, t_min/double(STAMP_SEC), t_max/double(STAMP_SEC),
	   mAvgSampleTime, 1/mAvgSampleTime);

    if (!mFft) {
	mFft = new CFFT(1024);
	mFft->setHamming();
    }
    if (!mYt)  mYt  = new float[1024];

    mFft->rfft(xt, mYt);

    mYmin = mYmax = mYt[0];
    for (i = 1; i < 1024; i++) {
	if (mYt[i] < mYmin) mYmin = mYt[i];
	if (mYt[i] > mYmax) mYmax = mYt[i];
    }

    maxValues(mYt, 512, mYPeak, mYPeakIx, N_PEAK_FREQUENCY);

    printf("Hz = [ ");
    for (i = 0; i < N_PEAK_FREQUENCY; i++) {
	printf(" %f[%d]", (1/mAvgSampleTime)/1024*mYPeakIx[i],mYPeakIx[i]);
    }
    printf("]\n");


display_old:
    y_mid   = mYmax + mYmin;
    y_range = mYmax - mYmin;

    memset(isPeak, 0, sizeof(isPeak));
    for (i = 0; i < N_PEAK_FREQUENCY; i++) isPeak[mYPeakIx[i]] = true;

    // Calcuate zero position
    y0 = int( ((0-mYmin)/y_range)*(aContext->cHeight-1) + aContext->lTop);
    // draw y into view, note that only half of the spectrum values are used
    // since the frequency spectrum is mirrored (hence the nyquist criterion)
    //
    i = 0;
    for (x = x0; x <= x1 && i < 512; x++, i++) {
	float v = mYt[i];
	int y;

	v = y_mid - v;
	y = int( ((v - mYmin)/y_range)*(aContext->cHeight-1) + aContext->lTop);

	switch(mPlotType.value()) {
	case PLOT_DOT:
	    epx_pixmap_draw_point(aContext->mPixmap, aContext->mGc, x, y);
	    break;
	case PLOT_RECTANGLE:
	case PLOT_LINE:
	case PLOT_SPIKE:
	    if (isPeak[i]) {
		epx_pixel_t save  = aContext->mGc->foreground_color;
		u_int8_t alpha = save.a;
		
		aContext->mGc->foreground_color.px = ~save.px;
		aContext->mGc->foreground_color.a  = alpha;
		epx_pixmap_draw_line(aContext->mPixmap, aContext->mGc, x, y0, x, y);
		aContext->mGc->foreground_color = save;
	    }
	    else
		epx_pixmap_draw_line(aContext->mPixmap, aContext->mGc, x, y0, x, y);
	    break;
	}
    }
}

void CPlotComponent::draw_samples(CRedrawContext *aContext,
				  CSamplerBase*   aSamples)
{
    int         maxn = aSamples->sampleCount();
    int         n    = maxn;
    int         ix   = aSamples->index() - 1;
    CSampleData* dp;
    float       maxv = aSamples->maxValue();
    float       minv = aSamples->minValue();
    double      tw;  // time range
    float       vw;  // value range
    TimeStamp   t;
    TimeStamp   t0;
    int         y0;
    int         x1=0, y1=0;
    bool first = true;

    tw   = (mPlotTime.value()*aContext->mHscale)*STAMP_SEC;
    vw   = (maxv - minv)*aContext->mVscale;

    if (ix < 0) ix = maxn - 1;
    dp   = aSamples->sample(ix);
    if (!dp)
	return;

    t     = dp->ts();
    t0 = (TimeStamp) (((double)t < tw) ? 0 : ((double) t - tw));

    y0 = int(((0-minv)/vw)*aContext->cHeight + aContext->lTop);

    // Draw from right to left
    while(n && (t >= t0)) {
	float v = dp->val();
	int   x;
	int   y;

	// change y axis and limit to min max
	v = (maxv+minv) - ((v > maxv) ? maxv : ((v < minv) ? minv : v));

	// scale the t1,v into x,y coordinate 
	x = int( ((t - t0)/tw)*(aContext->cWidth-1) + aContext->lLeft);
	y = int( ((v - minv)/vw)*(aContext->cHeight-1) + aContext->lTop);
	
	switch(mPlotType.value()) {
	case PLOT_DOT:
	    epx_pixmap_draw_point(aContext->mPixmap, aContext->mGc, x, y);
	    break;
	case PLOT_SPIKE:
	    epx_pixmap_draw_line(aContext->mPixmap, aContext->mGc, x, y0, x, y); 
	    break;
	case PLOT_LINE:
	    if (!first)
		epx_pixmap_draw_line(aContext->mPixmap, aContext->mGc, x, y, x1, y1);
	    break;
	case PLOT_RECTANGLE:
	    if (!first) {
		if (y > y0) {
		    epx_pixmap_draw_rectangle(aContext->mPixmap, aContext->mGc,
					      x, y0, (x1-x), (y-y0));
		}
		else if (y < y0) {
		    epx_pixmap_draw_rectangle(aContext->mPixmap, aContext->mGc,
					      x, y, (x1-x), (y0-y));
		}
	    }
	    break;
	}

	x1 = x;
	y1 = y;
	first = false;
	ix--;
	if (ix < 0) ix = maxn - 1;
	dp = aSamples->sample(ix);
	t  = dp->ts();
	n--;
    }
}


void CPlotComponent::redraw(CSystem* aSys, CRedrawContext *aContext)
{
    epx_gc_t* gc;
    u_int8_t         fader;
    CSamplerBase*    samples;
    epx_pixel_t         color;
    epx_pixel_t         bcolor;

    if (!aContext || !aContext->mPixmap) {
	WARNFMT("CPlotComponent::redraw(): No context or pixmap provided.");
	return;
    }

    samples = (CSamplerBase*) at(XINDEX(CPlotComponent,samples)).o;
    if (samples == NULL) {
	WARNFMT("CPlotComponent::redraw(): No samples provided.");
	return;
    }

    gc = aContext->mGc;
    fader = gc->fader_value;

    epx_gc_set_fill_style(gc, EPX_FILL_STYLE_BLEND|EPX_FILL_STYLE_AALIAS);
    epx_gc_set_border_width(gc, mBorderWidth.value());
    epx_gc_set_border_style(gc, EPX_BORDER_STYLE_AALIAS);

    color.px = mColor.value();
    color.a  = 255; // FIXME

    bcolor.px = mBorderColor.value();
    bcolor.a  = 255; // FIXME

    if (fader != ALPHA_FACTOR_1) {
	color.a = (color.a * fader) >> 8;
	bcolor.a   = (bcolor.a * fader) >> 8;
    }
    epx_gc_set_fill_color(gc,  color);
    epx_gc_set_foreground_color(gc,  color);
    epx_gc_set_border_color(gc, bcolor);
    
    if (mViewType.value() == VIEW_SAMPLES)
	draw_samples(aContext, samples);
    else if (mViewType.value() == VIEW_FREQUENCY)
	draw_frequency(aContext, samples);
}


