OSNAME := $(shell uname -s)
MACHINE := $(shell uname -m)

ERLDIR	:= /usr/local/lib/erlang.r9/erlang
ERL_C_INCLUDE_DIR := $(ERLDIR)/usr/include

X11       = No
MAC_OS    = No
WIN32     = No

ifeq ($(TYPE), debug)
EPIC_LIB=../../../lib/debug/epic.a
CFLAGS += -Ddebug
endif

ifeq ($(TYPE), release)
EPIC_LIB=../../../lib/release/epic.a
CFLAGS += -O3
endif


ifeq ($(OSNAME), Liunx)
FB = Yes
X11 = Yes
DLL_LIBS=
endif

ifeq ($(OSNAME), Darwin)
MAC_OS = Yes
X11 = Yes
LD_SHARED	:= cc -bundle -flat_namespace -undefined suppress
DLL_LIBS=
endif

LD_FLAGS = 

ifeq ($(X11), Yes)
	LD_FLAGS += -L/usr/X11R6/lib -lX11
endif

ifeq ($(MAC_OS),Yes)
	LD_FLAGS += -framework Carbon -framework CoreFoundation -framework Foundation -framework OpenGL -framework AGL
endif

DRV_OBJS = epic.o

EPIC_DRV_SO = ../priv/epic.so

WARN = -Wall -Wextra -Wswitch-default -Wswitch-enum 

override CFLAGS = $(WARN) -g -I ../../../include -I$(ERL_C_INCLUDE_DIR) -fPIC

all:
	$(MAKE) TYPE=release driver

debug:
	$(MAKE) TYPE=debug driver

release:
	$(MAKE) TYPE=release driver

# Make demo
driver: $(EPIC_DRV_SO)

clean:
	-rm -f $(EPIC_DRV_SO)

epic.o:	../../../include/epic.h

$(EPIC_DRV_SO): $(DRV_OBJS)
	$(LD_SHARED) -o $@ $(DRV_OBJS) $(DLL_LIBS) $(BE_LD_FLAGS) $(EPIC_LIB) $(LD_FLAGS) 

%.o:	%.c
	$(CC) -c -o$@ $(CFLAGS) $<