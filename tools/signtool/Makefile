#
# Makefile for the signtool
#
include ../../Version.inc
UNAME := $(shell uname)

CORE_DIR = ../../
BINDIR := ../../../bin
KEYTOOL_TARGET := $(BINDIR)/keytool
SIGNTOOL_TARGET := $(BINDIR)/signtool

CORE_INCDIR = $(CORE_DIR)/include
EPIC_INCDIR = $(CORE_DIR)/epic/include

RT_LIB = $(CORE_DIR)/obj/debug/m1rt.a

CC = $(CXX)

ifeq "$(UNAME)" "Darwin"
OS_CFLAGS= -DDARWIN=1  -DVERSION=\"$(VERSION)\" -I/opt/local/include
OS_LDFLAGS=-Wl,-search_paths_first
else
OS_CFLAGS=-I../../extern/include -DVERSION=\"$(VERSION)\"  -static
OS_LDFLAGS=-static
endif

CFLAGS = -g -Wall -I$(CORE_INCDIR) -I$(EPIC_INCDIR) $(OS_CFLAGS)
ifeq "$(UNAME)" "Darwin"
LIBS = -L../../extern/lib -lcrypto -lm -lz
else
LIBS = -L../../extern/lib -lcrypto -lm -ldl -lz
endif
all: $(KEYTOOL_TARGET) $(SIGNTOOL_TARGET)

$(KEYTOOL_TARGET): keytool.o $(RT_LIB)
	$(CXX)  $(OS_LDFLAGS) -o $@  keytool.o $(RT_LIB) $(LIBS)

$(SIGNTOOL_TARGET): signtool.o  $(RT_LIB)
	$(CXX)  $(OS_LDFLAGS) -o $@ signtool.o $(RT_LIB) $(LIBS)

# keytool.o signtool.o: make depend to m1rt library?

clean: 
	rm -f $(KEYTOOL_TARGET) $(SIGNTOOL_TARGET) keytool.o signtool.o *~

$(RT_LIB):
	(cd ../../src; make rtlib)