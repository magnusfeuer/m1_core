#
# Make the epic library
#
OSNAME := $(shell uname -s)
MACHINE := $(shell uname -m)
RANLIB = ranlib

X11       = No
FB        = No
MAC_OS_X  = No
MAC_OS_GL = No
WIN32     = No
ALTIVEC   = No
MMX       = No
SSE       = No
SSE2      = No
SSE3      = No
SSE4      = No

ifeq ($(TYPE), debug)
OBJDIR=../obj/debug
LIBDIR=../lib/debug
CFLAGS += -Ddebug -g
endif

ifeq ($(TYPE), release)
OBJDIR=../obj/release
LIBDIR=../lib/release
CFLAGS += -O3 
endif

ifeq ($(OSNAME), Linux)
FB = Yes
X11 = No
endif

ifeq ($(OSNAME), Darwin)
MAC_OS_X  = Yes
MAC_OS_GL = Yes
X11 = Yes
endif

ifeq ($(WITH_X11), 1)
X11 = Yes
endif 

ifeq ($(MACHINE), "Power Macintosh")
ALTIVEC   = Yes
endif

ifeq ($(MACHINE), i386)
MMX       = No
SSE       = No
SSE2      = Yes
SSE3      = No
endif

ifeq ($(MACHINE), i686)
MMX       = No
SSE       = No
SSE2      = Yes
SSE3      = No
endif

BE_C_FLAGS  =
BE_OBJS     =  epic_none.o
BE_LD_FLAGS = 

ifeq ($(X11), Yes)
	BE_C_FLAGS += -DX11
	BE_OBJS += epic_x11.o
	BE_LD_FLAGS += -L/usr/X11R6/lib -lX11
endif

ifeq ($(MAC_OS_X),Yes)
	BE_C_FLAGS += -DMAC_OS_X
	BE_OBJS += epic_macos.o
	BE_LD_FLAGS += -framework Carbon -framework CoreFoundation -framework Foundation 
endif

ifeq ($(MAC_OS_GL),Yes)
	BE_C_FLAGS += -DMAC_OS_GL
	BE_OBJS += epic_macos_gl.o
	BE_LD_FLAGS += -framework Carbon -framework CoreFoundation -framework Foundation -framework OpenGL -framework AGL
endif

ifeq ($(WIN32),Yes)
	BE_C_FLAGS += -DWIN32
	BE_OBJS += epic_win32.o
endif

ifeq ($(FB),Yes)
	BE_OBJS += epic_fb.o
	BE_C_FLAGS += -DFB
endif

MCFLAGS =
SIMD_OBJS = epic_simd_emu.o


ifeq ($(ALTIVEC),Yes)
	MCFLAGS += -maltivec -DUSE_ALTIVEC
	SIMD_OBJS += epic_simd_altivec.o
endif

ifeq ($(SSE),Yes)
	MCFLAGS += -msse
endif

ifeq ($(SSE2),Yes)
	MCFLAGS += -msse2 -DUSE_SSE2
	SIMD_OBJS += epic_simd_sse2.o
endif

ifeq ($(SSE3),Yes)
	MCFLAGS += -msse3
endif

ifeq ($(MMX),Yes)
	MCFLAGS += -mmmx -DUSE_MMX
	SIMD_OBJS += epic_simd_mmx.o
endif

LIB_OBJS = \
	epic_hash.o \
	epic_gc.o \
	epic_line.o \
	epic_ellipse.o \
	epic_pixel.o \
	epic_pixmap.o \
	epic_window.o \
	epic_backend.o \
	epic_bitmap.o \
	epic_dict.o \
	epic_font.o \
	epic_anim.o \
	epic_simd.o

EPIC_LIB = $(LIBDIR)/epic.a

EPIC_LIB_OBJS  = $(LIB_OBJS:%=$(OBJDIR)/%)
EPIC_SIMD_OBJS = $(SIMD_OBJS:%=$(OBJDIR)/%)
EPIC_BE_OBJS   = $(BE_OBJS:%=$(OBJDIR)/%)


WARN = -Wall -Wextra -Wswitch-default -Wswitch-enum 

override CFLAGS += \
	$(WARN) -g -I ../include $(MCFLAGS) $(BE_C_FLAGS) -I$(ERL_C_INCLUDE_DIR) -fPIC

all:	release


clean: 
	$(MAKE) clean_internal TYPE=release

# Make epic library
debug:
	$(MAKE) lib TYPE=debug

clean_debug:
	$(MAKE) clean_internal TYPE=debug

release:
	$(MAKE) lib TYPE=release

lib:	$(EPIC_LIB)

depend:

clean_internal:
	-rm -f $(EPIC_LIB) $(OBJDIR)/*.o


$(EPIC_LIB_OBJS): 	../include/epic.h epic_simd.h
$(EPIC_BE_OBJS):	../include/epic.h
$(OBJDIR)/epic_simd.o:    ../include/epic.h epic_simd.h
$(OBJDIR)/epic_simd_mmx.o: ../include/epic.h epic_simd.h epic_simd_mmx.h
$(OBJDIR)/epic_simd_sse2.o: ../include/epic.h epic_simd.h epic_simd_sse2.h
$(OBJDIR)/epic_simd_altivec.o: ../include/epic.h epic_simd.h epic_simd_altivec.h
$(OBJDIR)/epic_simd_emu.o: ../include/epic.h epic_simd.h epic_simd_emu.h

$(EPIC_LIB): $(EPIC_LIB_OBJS) $(EPIC_BE_OBJS) $(EPIC_SIMD_OBJS)
	rm -f $@
	$(AR) rcv $@ $(EPIC_LIB_OBJS) $(EPIC_BE_OBJS) $(EPIC_SIMD_OBJS)
ifdef RANLIB
	$(RANLIB) $@
endif

$(OBJDIR)/%.o:	%.c
	$(CC) -c -o$@ $(CFLAGS) $<


