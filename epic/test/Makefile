OSNAME := $(shell uname -s)
MACHINE := $(shell uname -m)

PNG_CFLAGS := $(shell libpng-config --I_opts)
PNG_LIBS   := $(shell libpng-config --libs)
PNG_LFLAGS := $(shell libpng-config --ldflags)

X11       = No
FB        = No
MAC_OS_X  = No
MAC_OS_GL = No
WIN32     = No

ifeq ($(TYPE), debug)
EPIC_LIB=../lib/debug/epic.a -lm
CFLAGS += -Ddebug $(PNG_CFLAGS) 
endif

ifeq ($(TYPE), opt)
OBJDIR=../obj/release
EPIC_LIB=../lib/release/epic.a -lm
CFLAGS += -O3 $(PNG_CFLAGS)
endif


ifeq ($(OSNAME), Linux)
FB = Yes
X11 = Yes
endif

ifeq ($(OSNAME), Darwin)
MAC_OS_X  = Yes
MAC_OS_GL = Yes
X11 = Yes
endif

LD_FLAGS = 

ifeq ($(X11), Yes)
	LD_FLAGS += -L/usr/X11R6/lib  -lX11 -lGL
endif

ifeq ($(MAC_OS_X),Yes)
	LD_FLAGS += -framework Carbon -framework CoreFoundation -framework Foundation
ifeq ($(MAC_OS_GL),Yes)
	LD_FLAGS += -framework AGL -framework OpenGL
	LD_FLAGS += -Wl,-dylib_file,/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib:/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib
endif
endif

ifeq ($(MAC_OS_X),Yes)
	LD_FLAGS += -sectcreate __TEXT __info_plist epic.plist
endif



DEMO_OBJS = $(LIB_OBJS) epic_demo.o

BENCH_OBJS = $(LIB_OBJS) epic_bench.o

WARN = -Wall -Wextra -Wswitch-default -Wswitch-enum 

override CFLAGS += $(WARN) -g -I ../include 

all:
	$(MAKE) TYPE=opt demo
	$(MAKE) TYPE=opt bench

debug:
	$(MAKE) TYPE=debug demo
	$(MAKE) TYPE=debug bench

opt:
	$(MAKE) TYPE=opt demo
	$(MAKE) TYPE=opt bench

# Make demo
demo: epic_demo

# Make bench
bench:	epic_bench

depend:

clean:
	-rm -f epic_demo epic_bench  *.o

epic_demo.o:	../include/epic.h
epic_bench.o:	../include/epic.h

epic_demo:	$(DEMO_OBJS)
	$(CC) -o $@  $(DEMO_OBJS) $(LD_FLAGS) $(PNG_LFLAGS) $(PNG_LIBS) $(EPIC_LIB)

epic_bench:	$(BENCH_OBJS) 
	$(CC) -o $@  $(BENCH_OBJS) $(LD_FLAGS) $(EPIC_LIB)