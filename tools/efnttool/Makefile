#
# Make file efnttool
#
include ../../Version.inc

UNAME := $(shell uname)
BINDIR := ../../../bin
TARGET := $(BINDIR)/efnttool
FREETYPE :=  freetype-2.3.5
CONFIGURED_FREETYPE := $(FREETYPE)/builds/unix/config.status
UNPACKED_FREETYPE := $(FREETYPE)/README
BUILT_FREETYPE := lib/libfreetype.a
CC=gcc

PNG_CFLAGS := $(shell libpng-config --I_opts)
PNG_LIBS   := $(shell libpng-config --libs)
PNG_LFLAGS := $(shell libpng-config --ldflags)

FT_CFLAGS  = $(shell ./bin/freetype-config --cflags)
FT_LIBS    = $(shell ./bin/freetype-config --libs)
FT_VERSION = $(shell ./bin/freetype-config --version)

CORE_DIR = ../..
EPIC_INCDIR = $(CORE_DIR)/epic/include
EPIC_LIB = $(CORE_DIR)/epic/lib/release/epic.a

ifeq "$(UNAME)" "Darwin"
CARBON_LIB=-framework Carbon -framework CoreFoundation -framework Foundation # -framework OpenGL -framework AGL
X11_LIB=-L/usr/X11R6/lib -lX11
OS_CFLAGS= -DDARWIN=1  -I/opt/local/include -mdynamic-no-pic
LDFLAGS:=-Wl,-search_paths_first
else
X11_LIB=-lX11
OS_CFLAGS= -rdynamic
LDFLAGS:=-rdynamic 
endif

ifneq "$(FT_VERSION)" ""
CFLAGS = -g -Wall  -DVERSION=\"$(VERSION)\" -DHAVE_FREETYPE_2 $(FT_CFLAGS) -I$(EPIC_INCDIR) $(PNG_CFLAGS) $(OS_CFLAGS)
LIBS = $(FT_LIBS) $(X11_LIB) $(CARBON_LIB) $(EPIC_LIB) $(PNG_LIBS) $(PNG_LFLAGS) -lm -ldl 
else
CFLAGS = -g -Wall  -DVERSION=\"$(VERSION)\" -I$(EPIC_INCDIR) $(PNG_CFLAGS) $(OS_CFLAGS)
LIBS = $(X11_LIB) $(CARBON_LIB) $(EPIC_LIB) $(PNG_LIBS) $(PNG_LFLAGS) -lm -ldl
endif

.PHONY: all efnttool


all: $(EPIC_LIB) $(BUILT_FREETYPE) $(TARGET)

efnttool: $(TARGET)

$(TARGET):  efnttool.o
	$(CC) -o $@ efnttool.o $(LDFLAGS) $(LIBS) 


$(BUILT_FREETYPE): $(CONFIGURED_FREETYPE)
	(cd $(FREETYPE); $(MAKE); $(MAKE) install)
	$(MAKE) efnttool

$(CONFIGURED_FREETYPE): $(UNPACKED_FREETYPE)
	(cd $(FREETYPE); CFLAGS=" -DTT_CONFIG_OPTION_BYTECODE_INTERPRETER=1" ./configure --prefix=$(PWD))

$(UNPACKED_FREETYPE):
	tar xzf $(FREETYPE).tar.gz

$(EPIC_LIB):
	(cd $(CORE_DIR)/epic/src; $(MAKE));

clean:
	rm -rf $(TARGET)  efnttool.o $(FREETYPE) bin lib share include
