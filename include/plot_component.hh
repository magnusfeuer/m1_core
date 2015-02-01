//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#ifndef __PLOT_COMPONENT_HH__
#define __PLOT_COMPONENT_HH__

#include "sampler.hh"
#include "fft.hh"
#include "component.hh"

typedef enum {
    PLOT_DOT       = 'd',
    PLOT_LINE      = 'l',
    PLOT_RECTANGLE = 'r',
    PLOT_SPIKE     = 's'
} PlotType;

typedef enum {
    VIEW_SAMPLES   = 's',
    VIEW_FREQUENCY = 'f'
} ViewType;

ENUM_TYPE(CPlotType, "PlotType",
	  ENUMERATION(dot,        PLOT_DOT),
	  ENUMERATION(line,       PLOT_LINE),
	  ENUMERATION(rectangle,  PLOT_RECTANGLE),
	  ENUMERATION(spike,      PLOT_SPIKE)
    );

ENUM_TYPE(CViewType, "ViewType",
	  ENUMERATION(samples,    VIEW_SAMPLES),
	  ENUMERATION(frequency,  VIEW_FREQUENCY)
    );

#define N_PEAK_FREQUENCY 8  // number of peaks to save
//
// A Shape component.
//
class CPlotComponent: public CLayerComponent {
public:
    XDERIVED_OBJECT_TYPE(CPlotComponent,CLayerComponent,
			 "Plot",
			 "Plot component",
			 (CPlotComponent_samples,
			  CPlotComponent_plotType,
			  CPlotComponent_viewType,
			  CPlotComponent_plotTime,
			  CPlotComponent_color,
			  CPlotComponent_borderWidth,
			  CPlotComponent_borderColor),
			 //! Sample buffer
			 XFIELD(CPlotComponent,Q_PUBLIC,samples,
				CSamplerBase::CSamplerBaseType::singleton(),
				"Sample buffer"),
			 //! Plot rendering type
			 XFIELD(CPlotComponent,Q_PUBLIC,plotType,
				CEventType::create(CPlotTypeType::singleton(),E_INPUT),
				"Plot rendering type, dot,line,rectangle or spike"),
			 //! View type
			 XFIELD(CPlotComponent,Q_PUBLIC,viewType,
				CEventType::create(CViewTypeType::singleton(),E_INPUT),
				"View type, samples or frequency"),
			 //! X axis time scale (time window)
			 XFIELD(CPlotComponent,Q_PUBLIC,plotTime,
				input_float_type(),
				"X axis time scale (time window)"),
			 //! Plot color
			 XFIELD(CPlotComponent,Q_PUBLIC,color,
				input_unsigned_type(),
				"Plot line color"),
			 //! Plot view border and border color
			 XFIELD(CPlotComponent,Q_PUBLIC,borderWidth,
				input_unsigned_type(),
				""),
			 XFIELD(CPlotComponent,Q_PUBLIC,borderColor,
				input_unsigned_type(),
				"")
	);
public:
    CPlotComponent(CExecutor* aExec,
		   CBaseType *aType = CPlotComponentType::singleton());
    ~CPlotComponent(void);

    void redraw(CSystem* aSys, CRedrawContext *aContext);

    void execute(CExecutor* aExec);

private:
    bool resample(CSamplerBase* aSample, float* x, size_t xn,
		  float &y_min, float &y_max, 
		  TimeStamp &t_min, TimeStamp &t_max, 
		  double &t_sample);
    int  maxValues(float* y, size_t yn, float* maxy, int* iy, int ym);
    void draw_frequency(CRedrawContext *aContext, CSamplerBase* aSamples);
    void draw_samples(CRedrawContext *aContext, CSamplerBase* aSamples);

    int mSamplerIndex;
    EventSigned   mPlotType;
    EventSigned   mViewType;
    EventFloat    mPlotTime;
    EventUnsigned mColor;
    EventUnsigned mBorderWidth;
    EventUnsigned mBorderColor;

    CFFT* mFft;
    // Data stored from last analyisis, for redraw
    unsigned mFftCount;  // call counter to reduce load
    float* mYt;
    float  mYmin;
    float  mYmax;
    double mAvgSampleTime;
    float  mYPeak[N_PEAK_FREQUENCY];
    int    mYPeakIx[N_PEAK_FREQUENCY];
    
};

#endif

