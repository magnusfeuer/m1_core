#
# All rights reserved. Reproduction, modification, use or disclosure
# to third parties without express authority is forbidden.
# Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
#
#
# Makefile for packfile tar.
#
include ../../Version.inc

CXX := g++
# leftmost number corresponds to Format-Version number

BINDIR := ../../../bin
UNAME := $(shell uname)

TARGET := $(BINDIR)/patchkey
OBJ =	 patchkey.o

ifeq "$(UNAME)" "Darwin"
CXXFLAGS:= -g  -DVERSION=\"$(VERSION)\" -I../../include 
LIBS:= -lcrypto 
else
CXXFLAGS:= -static -g  -DVERSION=\"$(VERSION)\"  -I../../include -I../../extern/include
LIBS:= -L../../extern/lib -lcrypto -ldl 
endif



$(TARGET): $(OBJ)
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(OBJ) $(LIBS)

%.o: %.c
	$(CXX) -c $(CXXFLAGS) -o $@ $<

clean:
	rm -f $(TARGET) $(PFPACKOBJ)

spotless: clean
	rm -rf config.status config.log configure autom4te.cache *~

