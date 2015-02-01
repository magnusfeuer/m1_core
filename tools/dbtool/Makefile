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

DBDUMP_TARGET := $(BINDIR)/dbdump
DBDUMP_OBJ =	 dbdump.o

ifeq "$(UNAME)" "Darwin"
CXXFLAGS:= -g  -DVERSION=\"$(VERSION)\"  -I../../include
else
CXXFLAGS:= -g  -DVERSION=\"$(VERSION)\"  -I../../include -static
endif

all: $(DBDUMP_TARGET)
$(DBDUMP_TARGET): $(DBDUMP_OBJ) database.o
	$(CXX) $(CXXFLAGS) -o $(DBDUMP_TARGET) $(DBDUMP_OBJ) database.o

%.o: %.c
	$(CXX) -c $(CXXFLAGS) -o $@ $<

clean:
	rm -f $(DBDUMP_TARGET) $(DBDUMP_OBJ) database.o

spotless: clean
	rm -rf config.status config.log configure autom4te.cache *~

database.o: ../../src/database.cc ../../include/database.hh
	$(CXX) -c $(CXXFLAGS) -DSTANDALONE -o database.o ../../src/database.cc

