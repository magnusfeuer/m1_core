// 
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005.
//

#include <stdint.h>
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "m1.hh"
#include "epic.h"
#if BYTE_ORDER == BIG_ENDIAN
/* MUST BE DEFINED FOR AVUTIL to work !!! */
#define WORDS_BIGENDIAN  
#endif

#include "video_component.hh"

XOBJECT_TYPE_BOOTSTRAP(CVideoComponent);

#define STREAM_FRAME_RATE    25             /* 25 images/s */
#define STREAM_FRAME_WIDTH   640
#define STREAM_FRAME_HEIGHT  480
#define STREAM_PIX_FMT PIX_FMT_YUV420P   /* default pix_fmt */

static AVFrame *allocVideoFrame(int pix_fmt, int width, int height)
{
    AVFrame *frame;
    u_int8_t *frame_buf;
    int size;

    if ((frame = avcodec_alloc_frame()) == NULL)
	return NULL;
    size = avpicture_get_size(pix_fmt, width, height);
    if ((frame_buf = (u_int8_t*) av_malloc(size)) == NULL) {
        av_free(frame);
        return NULL;
    }
    avpicture_fill((AVPicture *)frame, frame_buf,
                   pix_fmt, width, height);
    return frame;
}

#ifdef USE_FFMPEG
//
// Write a frame - return true iff the frame was buffered 
//
static bool outputFrame(VideoCtx* ctx)
{
    AVStream *st;
    AVCodecContext *c;

    if (!ctx->is_open || (ctx->video_index < 0))
	return false;

    st = ctx->format->streams[ctx->video_index];
    c = st->codec;

    if (ctx->format->oformat->flags & AVFMT_RAWPICTURE) {
        /* raw video case. The API will change slightly in the near
           futur for that */
        AVPacket pkt;
        av_init_packet(&pkt);

        pkt.flags |= PKT_FLAG_KEY;
        pkt.stream_index= st->index;
        pkt.data= (uint8_t *)ctx->frame;
        pkt.size= sizeof(AVPicture);
        if (av_write_frame(ctx->format, &pkt) != 0)
	    ERRFMT("VideoComponent: error while writing frame");
	return false;
    }
    else {
	int sz = avcodec_encode_video(c, ctx->buffer, ctx->buffer_size,
				      ctx->frame);
	if (sz > 0) {
	    AVPacket pkt;
	    av_init_packet(&pkt);

	    pkt.pts= av_rescale_q(c->coded_frame->pts, c->time_base, 
				  st->time_base);
	    if(c->coded_frame->key_frame)
		pkt.flags |= PKT_FLAG_KEY;
	    pkt.stream_index = st->index;
	    pkt.data = ctx->buffer;
	    pkt.size = sz;

	    /* write the compressed frame in the media file */
	    if (av_write_frame(ctx->format, &pkt) != 0)
		ERRFMT("VideoComponent: error while writing frame");
	    return false;
	} 
	return true;
    }
}
#endif


static void initVideoCtx(VideoCtx* ctx)
{
    ctx->is_open = false;
    ctx->is_buffered = false;
    ctx->video_index = -1;
    ctx->audio_index = -1;
#ifdef USE_FFMPEG
    ctx->format = NULL;
    ctx->frame = NULL;
    ctx->buffer = NULL;
    ctx->buffer_size = 0;
    ctx->frame_count = 0;
#endif
}

#ifdef USE_FFMPEG
static void closeVideoCtx(VideoCtx* ctx, VideoMode mode)
{
    ctx->is_open = false;

    if (ctx->format) {
	int i;

	if (mode == VIDEO_OUT) {
	    if (ctx->is_buffered) {
		int n = 100;
		while(n && outputFrame(ctx))
		    n--;
	    }
	    av_write_trailer(ctx->format);
	}

	if (ctx->video_index >= 0) {
	    AVStream* st = ctx->format->streams[ctx->video_index];
	    avcodec_close(st->codec);
	    ctx->video_index = -1;
	}

	if (ctx->audio_index >= 0) {
	    AVStream* st = ctx->format->streams[ctx->audio_index];
	    avcodec_close(st->codec);
	    ctx->audio_index = -1;
	}

	if (mode == VIDEO_IN)
	    av_close_input_file(ctx->format);

	/* free the streams */
	for(i = 0; i < (int) ctx->format->nb_streams; i++) {
	    av_freep(&ctx->format->streams[i]->codec);
	    av_freep(&ctx->format->streams[i]);
	}

	if ((mode == VIDEO_OUT) && !(ctx->format->flags & AVFMT_NOFILE)) {
	    url_fclose(&ctx->format->pb);
	}
	av_free(ctx->format);
    }

    if (ctx->frame) {
	if (mode == VIDEO_OUT)
	    av_free(ctx->frame->data[0]);
	av_free(ctx->frame);
	ctx->frame = NULL;
    }

    if (ctx->buffer) {
	av_free(ctx->buffer);
	ctx->buffer = NULL;
    }
    ctx->buffer_size = 0;
    ctx->frame_count = 0;
}
#endif


/* add a video output stream */
#ifdef USE_FFMPEG
static void addVideoStream(VideoCtx* ctx, CodecID codec_id)
{
    AVFormatContext *oc = ctx->format;
    AVCodecContext *c;
    AVStream *st;

    ctx->video_index = -1;
    if ((st = av_new_stream(oc, 0)) == NULL) {
        ERRFMT("VideoComponent: Could not alloc stream");
	return;
    }
    ctx->video_index = st->index;

    c             = st->codec;
    c->codec_id   = codec_id;
    c->codec_type = CODEC_TYPE_VIDEO;

    /* put sample parameters */
    c->bit_rate = 400000;
    /* resolution must be a multiple of two */
    c->width    = STREAM_FRAME_WIDTH;
    c->height   = STREAM_FRAME_HEIGHT;
    /* time base: this is the fundamental unit of time (in seconds) in terms
       of which frame timestamps are represented. for fixed-fps content,
       timebase should be 1/framerate and timestamp increments should be
       identically 1. */
    c->time_base.num = 1;
    c->time_base.den = STREAM_FRAME_RATE;
    c->gop_size = 12; /* emit one intra frame every twelve frames at most */
    c->pix_fmt = STREAM_PIX_FMT;
    if (c->codec_id == CODEC_ID_MPEG2VIDEO) {
        /* just for testing, we also add B frames */
        c->max_b_frames = 2;
    }
    if (c->codec_id == CODEC_ID_MPEG1VIDEO){
        /* needed to avoid using macroblocks in which some coeffs overflow
           this doesnt happen with normal video, it just happens here as the
           motion of the chroma plane doesnt match the luma plane */
        c->mb_decision=2;
    }
    // some formats want stream headers to be separate
    if(!strcmp(oc->oformat->name, "mp4") || !strcmp(oc->oformat->name, "mov") || !strcmp(oc->oformat->name, "3gp"))
        c->flags |= CODEC_FLAG_GLOBAL_HEADER;
}
#endif
/*
 * add an audio output stream
 */
#ifdef USE_FFMPEG
static void addAudioStream(VideoCtx* ctx, CodecID codec_id)
{
    AVFormatContext *oc = ctx->format;
    AVCodecContext *c;
    AVStream *st;

    ctx->audio_index = -1;
    if ((st = av_new_stream(oc, 1)) == NULL) {
        ERRFMT("VideoComponent: Could not alloc audio stream");
	return;
    }

    ctx->audio_index = st->index;

    c             = st->codec;
    c->codec_id   = codec_id;
    c->codec_type = CODEC_TYPE_AUDIO;

    /* put sample parameters */
    c->bit_rate    = 64000;
    c->sample_rate = 44100;
    c->channels    = 2;
}
#endif

#ifdef USE_FFMPEG
static bool openVideo(VideoCtx* ctx)
{
    AVCodec *codec;
    AVCodecContext *c;
    AVStream *st;

    if ((ctx->format == NULL) || (ctx->video_index < 0))
	return false;
    st = ctx->format->streams[ctx->video_index];
    c = st->codec;

    /* find the video encoder */
    if ((codec = avcodec_find_encoder(c->codec_id)) == NULL) {
	ERRFMT("VideoComponent: video codec not found");
	return false;
    }

    /* open the codec */
    if (avcodec_open(c, codec) < 0) {
	ERRFMT("VideoComponent: could not open video codec");
	return false;
    }

    if (!(ctx->format->oformat->flags & AVFMT_RAWPICTURE)) {
        /* allocate output buffer */
        /* XXX: API change will be done */
        /* buffers passed into lav* can be allocated any way you prefer,
           as long as they're aligned enough for the architecture, and
           they're freed appropriately (such as using av_free for buffers
           allocated with av_malloc) */
        ctx->buffer_size = 200000;
	ctx->buffer = (u_int8_t*) av_malloc(ctx->buffer_size);
    }

    if ((ctx->frame = allocVideoFrame(c->pix_fmt, c->width, c->height)) == NULL) {
	ERRFMT("VideoComponent: Could not allocate picture");
	return false;
    }
    return true;
}
#endif

#ifdef USE_FFMPEG
static bool openAudio(VideoCtx* ctx)
{
    AVCodecContext *c;
    AVCodec *codec;
    AVStream *st;

    if ((ctx->format == NULL) || (ctx->audio_index < 0))
	return false;
    st = ctx->format->streams[ctx->audio_index];
    c = st->codec;

    /* find the audio encoder */
    if ((codec = avcodec_find_encoder(c->codec_id)) == NULL) {
        ERRFMT("VideoComponent: audio codec not found");
	return false;
    }

    /* open it */
    if (avcodec_open(c, codec) < 0) {
	ERRFMT("VideoComponent: could not open adio codec");
        return false;
    }
    return true;
}
#endif



CVideoComponent::CVideoComponent(CExecutor* aExec, CBaseType *aType):
    CLayerComponent(aExec, aType),
    mStreamName(this),
    mLoop(this),
    mMode(this),
    mFrameTime(this),
    mFrameRate(this),
    mChannel(this),
    mStandard(this),
    mInputFormatName(this),
    mBorderWidth(this),
    mBorderColor(this)    
{ 
    mLoop.putValue(aExec, false);
    mMode.putValue(aExec, VIDEO_IN);

    mRunning = false;
    mFrameLoaded = false;

#ifdef USE_FFMPEG
    initVideoCtx(&mInCtx);
    initVideoCtx(&mOutCtx);

    // AVFormatParameters - mMode = in|inout
    mChannel.putValue(aExec,  0);
    mStandard.putValue(aExec, "");         // "pal"|"ntsc"|"secam" ...
    mInputFormatName.putValue(aExec, "");  // video4linux | dv1394 ...

    av_register_all();
#endif
    eventPut(aExec,XINDEX(CVideoComponent,streamName), &mStreamName);
    eventPut(aExec,XINDEX(CVideoComponent,loop), &mLoop);
    eventPut(aExec,XINDEX(CVideoComponent,mode), &mMode);
    eventPut(aExec,XINDEX(CVideoComponent,frameTime), &mFrameTime);
    eventPut(aExec,XINDEX(CVideoComponent,frameRate), &mFrameRate);
    eventPut(aExec,XINDEX(CVideoComponent,channel), &mChannel);
    eventPut(aExec,XINDEX(CVideoComponent,standard), &mStandard);
    eventPut(aExec,XINDEX(CVideoComponent,format), &mInputFormatName);
    eventPut(aExec,XINDEX(CVideoComponent,borderWidth), &mBorderWidth);
    eventPut(aExec,XINDEX(CVideoComponent,borderColor), &mBorderColor);
}

CVideoComponent::~CVideoComponent(void)
{
    DBGFMT("VideoComponent::~CVideoComponent()");
    close();
}


// Read one frame 
bool CVideoComponent::readFrame()
{
#ifdef USE_FFMPEG
    AVStream        *st;
    AVCodecContext  *codec;
    AVPacket        packet;
    int             frameFinished;
    int             loop = 0;
    int             err;

    if (!mRunning || !mInCtx.is_open || (mInCtx.video_index < 0))
	return false;
    st = mInCtx.format->streams[mInCtx.video_index];
    codec  = st->codec;
    
    mFrameLoaded = false;
    
loop:
    while((err=av_read_frame(mInCtx.format, &packet)) >= 0) {
	// Is this a packet from the video stream?
	if (packet.stream_index != mInCtx.video_index) {
	    av_free_packet(&packet);
	    continue;
	}
	
	// Decode video frame
	avcodec_decode_video(codec, mInCtx.frame, &frameFinished, 
			     packet.data, packet.size);
	// Did we get a video frame?
	if (frameFinished) {
	    av_free_packet(&packet);
	    mFrameLoaded = true;
	    mInCtx.frame_count++;
	    return true;
	}
	av_free_packet(&packet);
    }
    if (mLoop.value() && !loop) {
	// printf("ReadFrame: err=%d, try loop\n", err);
	// Try seek from start. FIXME need to reopen for RTSP etc.
	loop = 1;
	av_seek_frame(mInCtx.format, mInCtx.video_index, 0LL, 0);
	mInCtx.frame_count = 0;
	goto loop;
    }
#endif

    mRunning = false;
    return false;
}

void CVideoComponent::writeFrame(EPixmap* aPixmap)
{
    AVStream *st;
    AVCodecContext *c;
    u_int8_t* src_data[4];
    int       src_linesize[4];
    int       sfmt;

    if (!mOutCtx.is_open || (mOutCtx.video_index < 0))
	return;

    sfmt = pixelType2PixelFormat(aPixmap->pixelType);
    st = mOutCtx.format->streams[mOutCtx.video_index];
    c = st->codec;
	
    mSwsContext2 = sws_getCachedContext(mSwsContext2, 
					aPixmap->width, aPixmap->height,
					sfmt,
					c->width, c->height,
					c->pix_fmt,
					SWS_BICUBIC,
					NULL, NULL, NULL);
    src_data[0] = aPixmap->data;
    src_data[1] = src_data[2] = src_data[3] = NULL;

    src_linesize[0] = aPixmap->bytesPerRow;
    src_linesize[1] = src_linesize[2] = src_linesize[3] = 0;

    sws_scale(mSwsContext2, src_data, src_linesize, 0,
	      c->height, mOutCtx.frame->data, mOutCtx.frame->linesize);

    if ((mOutCtx.is_buffered = outputFrame(&mOutCtx)) == false)
	mOutCtx.frame_count++;
}



void CVideoComponent::close(void) 
{
    mRunning = false;
    mFrameLoaded = false;
#ifdef USE_FFMPEG
    if (mInCtx.is_open)  closeVideoCtx(&mInCtx,  VIDEO_IN);
    if (mOutCtx.is_open) closeVideoCtx(&mOutCtx, VIDEO_OUT);
#endif
}

// Open input audio/video stream
bool CVideoComponent::openIn(CExecutor* aExec, VideoCtx* ctx)
{
#ifdef USE_FFMPEG
    AVCodecContext *c;
    AVCodec  *codec;
    AVInputFormat* iformat = NULL;
    AVFormatParameters* params_ptr = NULL;
    AVFormatParameters  params;    // Video input params
    char*    file_name = NULL;
    int      i;
    char*    streamName = (char*) mStreamName.value().c_str();
    struct stat st;    

    DBGFMT("VideoComponent::openIn()");

    ctx->is_open = false;
    memset(&params, 0, sizeof(params));
again:
    if (stat(streamName, &st) < 0) {
	// this may be an url 
	if (strncmp(streamName, "http://", 7) == 0) {
	    // Open url
	    ERRFMT("VideonComponent:: can not open url streams yet");
	    return false;
	}
	else if (strncmp(streamName, "file://", 7) == 0) {
	    streamName += 7;
	    goto again;
	}
	else {
	    ERRFMT("stream %s: open error: %s", streamName, strerror(errno));
	    return false;
	}
    }

    if (st.st_mode & (S_IFBLK|S_IFCHR)) {
	// Open as device
	params_ptr = &params;
	// params.device     = streamName;
	// params.channels   = mChannel;
	// params.standard   = mStandard.self().c_str();
	params.width      = (int) mContentWidth.value();
	params.height     = (int) mContentHeight.value();
	// FIXME convert frame rate float to rational
	params.time_base.num = 1;
	params.time_base.den = (int) mFrameRate.value(); 
	params.pix_fmt       = PIX_FMT_NONE;
	if (mInputFormatName.value().length() != 0)
	    iformat = av_find_input_format(mInputFormatName.value().c_str());
	file_name = (char*) streamName;
    }
    else if (st.st_mode & S_IFREG) {
	file_name = (char*) streamName;
    }
    else {
	ERRFMT("VideoComponent:%s: unknown file mode %3o", 
	       streamName, st.st_mode);
	return false;
    }

    // Open video file
    if (av_open_input_file(&ctx->format, file_name,
			   iformat, 0, params_ptr)!=0) {
	ERRFMT("VideoComponent:%s: could not be opened for reading",
	       streamName);
	return false;
    }

    // Retrieve stream information
    if (av_find_stream_info(ctx->format)<0) {
	ERRFMT("VideoComponent:%s: can not find stream info", streamName);
	goto error_close;
    }

#ifdef DEBUG
    dump_format(ctx->format, 0, streamName, false);
#endif

    // Find the first video/audio stream
    i = 0;
    while((i < (int) ctx->format->nb_streams) &&
	  ((ctx->video_index==-1) || (ctx->audio_index==-1))) {
	AVStream *st = ctx->format->streams[i];
	if ((ctx->video_index == -1) && 
	    (st->codec->codec_type==CODEC_TYPE_VIDEO)) {
	    if(st->r_frame_rate.den && st->r_frame_rate.num)
		mFrameRate.putValue(aExec, av_q2d(st->r_frame_rate));
	    else
		mFrameRate.putValue(aExec, 1/av_q2d(st->codec->time_base));
	    ctx->video_index=i;
	}
	else if ((ctx->audio_index == -1) &&
		 (st->codec->codec_type==CODEC_TYPE_AUDIO)) {
	    // Bits/sec?
	    ctx->audio_index=i;
	}
	i++;
    }

    if (ctx->video_index==-1) {
	ERRFMT("VideoComponent:%s: no video found", streamName);
	goto error_close;
    }
	
    if (ctx->audio_index==-1) {
	// Just a warning
	ERRFMT("VideoComponent:%s: no audio found", streamName);
    }

    // Get a pointer to the codec context for the video stream
    c = ctx->format->streams[ctx->video_index]->codec;
    
    // Find the decoder for the video stream
    if ((codec=avcodec_find_decoder(c->codec_id)) == NULL) {
	ERRFMT("VideoComponent:%s: no codec found", streamName);
	goto error_close;
    }

    // Open codec
    if (avcodec_open(c, codec)<0) {
	ERRFMT("VideoComponent:%s: could not open codec", streamName);
	goto error_close;
    }
    
    // Allocate video frame
    if ((ctx->frame=avcodec_alloc_frame()) == NULL) {
	ERRFMT("VideoComponent:%s: out of memory", streamName);
	goto error_close;
    }

    ctx->is_open = true;
    return true;

error_close:
    closeVideoCtx(ctx, VIDEO_IN);
    return false;
#else
    ctx->is_open = false;
    return false;
#endif
}



bool CVideoComponent::openOut(CExecutor*, VideoCtx* ctx)
{
#ifdef USE_FFMPEG
    char* streamName = (char*) mStreamName.value().c_str();
    AVOutputFormat *oformat;
//    double audio_pts;
//    double video_pts;
//    int i;    

    DBGFMT("VideoComponent::openOut()");

    ctx->is_open = false;

    oformat = guess_format(NULL, streamName, NULL);
    if (!oformat) {
        DBGFMT("VideoComponent: could not guess from file name, using MPEG");
        oformat = guess_format("mpeg", NULL, NULL);
    }
    if (!oformat) {
	ERRFMT("VideoComponent: could not open %s", streamName);
	return false;
    }

    if ((ctx->format = av_alloc_format_context()) == NULL) {
	ERRFMT("VidoComponent: av_alloc_format_context failed");
	return false;
    }
	
    ctx->format->oformat = oformat;
    snprintf(ctx->format->filename, sizeof(ctx->format->filename), "%s", streamName);
    
    if (oformat->video_codec != CODEC_ID_NONE)
	addVideoStream(ctx, oformat->video_codec);
    if (oformat->audio_codec != CODEC_ID_NONE)
	addAudioStream(ctx, oformat->audio_codec);

    /* set the output parameters (must be done even if no
       parameters). */
    if (av_set_parameters(ctx->format, NULL) < 0) {
        ERRFMT("VideoComponent: Invalid output format parameters");
	goto error_close;
    }

    /* now that all the parameters are set, we can open the audio and
       video codecs and allocate the necessary encode buffers */
    openVideo(ctx);
    openAudio(ctx);
    
    /* open the output file, if needed */
    if (!(ctx->format->flags & AVFMT_NOFILE)) {
        if (url_fopen(&ctx->format->pb, streamName, URL_WRONLY) < 0) {
            ERRFMT("VideoComponent: Could not open '%s'", streamName);
	    goto error_close;
        }
    }

    /* write the stream header, if any */
    av_write_header(ctx->format);

    ctx->is_open = true;
    return true;

error_close:
    closeVideoCtx(ctx, VIDEO_OUT);
    return false;
#else
    ctx->is_open = false;
    return false;
#endif
}


bool CVideoComponent::open(CExecutor* aExec) 
{
#ifdef USE_FFMPEG
    if (mMode.value() & VIDEO_IN)   openIn(aExec, &mInCtx);
    if (mMode.value() & VIDEO_OUT)  openOut(aExec, &mOutCtx);

    if (!mInCtx.is_open && !mOutCtx.is_open)
	return false;

    if (mInCtx.is_open && (mInCtx.video_index >= 0)) {
	AVStream* st = mInCtx.format->streams[mInCtx.video_index];
	AVCodecContext *c = st->codec;
	mContentHeight.putValue(aExec, (float) c->height);
	mContentWidth.putValue(aExec,  (float) c->width);
    }
    else if (mOutCtx.is_open && (mOutCtx.video_index >= 0)) {
	AVStream* st = mOutCtx.format->streams[mOutCtx.video_index];
	AVCodecContext *c = st->codec;
	mContentHeight.putValue(aExec, (float) c->height);
	mContentWidth.putValue(aExec, (float) c->width);
    }
    mRunning = true;
    return true;
#endif
}

void CVideoComponent::start(CExecutor* aExec)
{
    if (mStreamName.assigned()) {
	if (mEnabled.value()) {
	    open(aExec);
	    readFrame();
	}
	// MUST clear flags here otherwise execute will run them again.
	mEnabled.cancel(aExec);
	mStreamName.cancel(aExec);
	mFrameTime.cancel(aExec); // just in case
    }
}


void CVideoComponent::execute(CExecutor* aExec)
{
    DBGFMT("VideoComponent::execute(): Called");

    if (mStreamName.updated()) {
	close();
	if (mEnabled.value())
	    open(aExec);
    }
    else if (mEnabled.updated()) {
	if (!mEnabled.value())
	    close();
	else {
	    open(aExec);
	    readFrame();
	}
    }

    if (mFrameTime.updated() || mStreamName.updated()) {
	readFrame();
    }
}

// Scale mFrame (always) and convert to destination format
EPixmap* CVideoComponent::scaleFrame(CRedrawContext *aContext)
{
#ifdef USE_FFMPEG
    u_int8_t* dst_data[4];
    int       dst_linesize[4];
    int       dfmt = pixelType2PixelFormat(aContext->mPixmap->pixelType);
    AVStream* st;
    AVCodecContext *c;
    int       sfmt;
    EPixmap*  dImage;
    AVFrame*  frame;
    SwsContext* sws;

    if (mInCtx.is_open && (mInCtx.video_index >= 0)) {
	st = mInCtx.format->streams[mInCtx.video_index];
	frame = mInCtx.frame;
    }
    else if (mOutCtx.is_open && (mOutCtx.video_index >= 0)) {
	st = mOutCtx.format->streams[mOutCtx.video_index];
	frame = mOutCtx.frame;
    }
    else
	return NULL;

    if (frame == NULL)
	return NULL;
    c    = st->codec;
    sfmt = (int) c->pix_fmt;

    if ((sws = cachedSwsContext(aContext, sfmt, dfmt, true)) == NULL)
	return NULL;

    // Fixme: we may want to cache the dImage ?
    if ((dImage = EPixmapCreate(int(aContext->cWidth),
				int(aContext->cHeight),
				aContext->mPixmap->pixelType)) == NULL)
	return NULL;

    dst_data[0] = dImage->data;
    dst_data[1] = dst_data[2] = dst_data[3] = NULL;

    dst_linesize[0] = dImage->bytesPerRow;
    dst_linesize[1] = dst_linesize[2] = dst_linesize[3] = 0;

    sws_scale(sws, frame->data, frame->linesize, 0,
	      int(mContentHeight.value()), dst_data, dst_linesize);
    return dImage;
#else
    return NULL;
#endif

}


void CVideoComponent::redraw(CSystem* aSys, CRedrawContext *aContext)
{
    EGc* gc;
    u_int8_t fader;
    EPixmap* sImage;
    EPixel_t color;

    if (!aContext || !aContext->mPixmap) {
	DBGFMT_WARN("CImageComponent::redraw(): No context provided.");
	return;
    }

    gc = aContext->mGc;
    fader = gc->fader_value;

    EGcSetBorderWidth(gc, mBorderWidth.value());

    if (mBorderWidth.value()) {
	color.px = mBorderColor.value();
	color.a  = 255;
	if (fader != ALPHA_FACTOR_1)
	    color.a = (color.a * fader) >> 8;
	EGcSetBorderColor(gc, color);
    }

    if ((sImage = scaleFrame(aContext)) == NULL)
	return;

    EPixmapAlphaArea(sImage, aContext->mPixmap,
		     fader,
		     0, 0,
		     int(aContext->lLeft), 
		     int(aContext->lTop),
		     sImage->width, sImage->height);
    if (sImage)
	EPixmapDestroy(sImage);
}
