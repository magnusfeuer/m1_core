//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005.
//

#ifndef __VIDEO_COMPONENT_H__
#define __VIDEO_COMPONENT_H__

#include <sys/types.h>

#include "epx.h"
#include "m1.hh"
#include "component.hh"


#ifdef USE_FFMPEG
#include <ffmpeg/avformat.h>
#endif

typedef enum {
    VIDEO_IN    = 0x1,   // default reading camera/file/url
    VIDEO_OUT   = 0x2,   // writing screen/layer to file
    VIDEO_INOUT = 0x3    // recording from camera to file/url
} VideoMode;

typedef struct {
    int             is_open;      //  in use
    int             is_buffered;  //  are there any data buffered
    int             video_index;  //  video stream index
    int             audio_index;  //  audio stream index
#ifdef USE_FFMPEG
    AVFormatContext *format;      //  format ctx
    AVFrame         *frame;       //  allocate frame
    u_int8_t*       buffer;       //  general video buffer
    size_t          buffer_size;   //  and it's size
    unsigned long   frame_count;   // number of frames read/written
#endif
} VideoCtx;

ENUM_TYPE(CVideoMode, "VideoMode",
	  ENUMERATION(in,   VIDEO_IN),
	  ENUMERATION(out,  VIDEO_OUT),
	  ENUMERATION(inout, VIDEO_INOUT));

//
// A Video component.
//
class CVideoComponent: public CLayerComponent {
public:
    XDERIVED_OBJECT_TYPE(CVideoComponent, CLayerComponent,
			 "Video",
			 "Video input and output component",
			 (CVideoComponent_streamName,
			  CVideoComponent_loop,
			  CVideoComponent_mode,
			  CVideoComponent_frameTime,
			  CVideoComponent_frameRate,
			  CVideoComponent_channel,
			  CVideoComponent_standard,
			  CVideoComponent_format,
			  CVideoComponent_borderWidth,
			  CVideoComponent_borderColor),
			 XFIELD(CVideoComponent,Q_PUBLIC,streamName,
				input_string_type(), 
				"File name of stream or device name."),
			 XFIELD(CVideoComponent,Q_PUBLIC,loop,
				input_bool_type(),
				"Loop at end frame (if possible)."),
			 XFIELD(CVideoComponent,Q_PUBLIC,mode,
				CEventType::create(CVideoModeType::singleton(),E_INPUT),
				"in/out/inout"),
			 XFIELD(CVideoComponent,Q_PUBLIC,frameTime,
				event_time_type(),
				"Last frame time"),
			 XFIELD(CVideoComponent,Q_PUBLIC,frameRate,
				event_float_type(),
				"Video frame rate (output)"),
			 
			 XFIELD(CVideoComponent,Q_PUBLIC,channel,
				input_unsigned_type(),
				"Channel number"),
			 XFIELD(CVideoComponent,Q_PUBLIC,standard,
				input_string_type(),
				"TV standard ntsc/pal"),
			 XFIELD(CVideoComponent,Q_PUBLIC,format,
				input_string_type(),
				"video4linux etc"),
			 XFIELD(CVideoComponent,Q_PUBLIC,borderWidth,
				input_unsigned_type(),
				"Border frame width"),
			 XFIELD(CVideoComponent,Q_PUBLIC,borderColor,
				input_unsigned_type(),
				"Color of the border frame")
	);
public:
    CVideoComponent(CExecutor* aExec,
		    CBaseType *aType = CVideoComponentType::singleton());
    ~CVideoComponent(void);

    void redraw(CSystem* aSys, CRedrawContext *aContext);

    void execute(CExecutor* aExec);
    void start(CExecutor* aExec);

    void writeFrame(epx_pixmap_t* aPixmap);
private:
    bool open(CExecutor* aExec);
    bool openIn(CExecutor* aExec, VideoCtx* ctx);
    bool openOut(CExecutor* aExec, VideoCtx* ctx);
    bool readFrame();
    void close();
    epx_pixmap_t* scaleFrame(CRedrawContext *aContext);

    EventString mStreamName;          // file name of stream or device name
    EventBool   mLoop;                // loop at end frame (if possible)
    EventEnum<VideoMode> mMode;             // in/out/inout
    EventUnsigned mFrameTime;     // last frame time
    EventFloat      mFrameRate;     // video frame rate (output)
    EventSigned     mChannel;             // Channel number
    EventString     mStandard;            // TV standard "ntsc"/"pal" ..
    EventString     mInputFormatName;     // for video4linux etc

    EventUnsigned mBorderWidth;
    EventUnsigned mBorderColor;

    bool    mRunning;                     // is stream running?
    bool    mFrameLoaded;                 // do we have a loaded frame?
    VideoCtx mInCtx;
    VideoCtx mOutCtx;
    int      mFormatWidth;
    int      mFormatHeight;
};



#endif // __VIDEO_COMPONENT_H__
