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

M1_RTLIB := ../../obj/debug/m1rt.a
PFLS_TARGET := $(BINDIR)/pfls
FS2DB_TARGET := $(BINDIR)/fs2db
DB2PF_TARGET := $(BINDIR)/db2pf
DB2FS_TARGET := $(BINDIR)/db2fs
FS2PF_TARGET := $(BINDIR)/fs2pf
PF2FS_TARGET := $(BINDIR)/pf2fs
PFINFO_TARGET := $(BINDIR)/pfinfo
FS2DB_OBJ =	 fs2db.o sqlcommon.o
DB2PF_OBJ =	 db2pf.o sqlcommon.o pfcommon.o
DB2FS_OBJ =	 db2fs.o sqlcommon.o pfcommon.o
FS2PF_OBJ =	 fs2pf.o sqlcommon.o pfcommon.o 
PF2FS_OBJ =	 pf2fs.o
PFLS_OBJ =	 pfls.o
PFINFO_OBJ =	 pfinfo.o


#
# -static needed for linux since some of these apps will run from a bootable USB stick.
#
ifeq "$(UNAME)" "Darwin"
CXXFLAGS:= -g -DVERSION=\"$(VERSION)\" -DDARWIN=1  -I/opt/local/include  -I../../include -I../sqltool  -DPRINT_ERR=1 # -DPRINT_DBG=1
LDFLAGS += -Wl,-search_paths_first -L/opt/local/lib/mysql -L../../extern/lib 
else
CXXFLAGS:= -g   -DVERSION=\"$(VERSION)\"  -I../../include -I../../extern/include -I../sqltool -static -DPRINT_ERR=1 -DPRINT_DBG=1
LDFLAGS += -static
endif

all: $(FS2DB_TARGET) $(DB2FS_TARGET) $(DB2PF_TARGET)  $(PFINFO_TARGET) $(PFLS_TARGET) $(FS2PF_TARGET) $(PF2FS_TARGET)

LIBS = -lz

phony: m1rtlib

$(FS2DB_TARGET): m1rtlib $(FS2DB_OBJ)
	$(CXX) $(CXXFLAGS) -o $(FS2DB_TARGET) $(FS2DB_OBJ) $(M1_RTLIB) $(LDFLAGS) -lcrypto -ldl -lmysqlclient -lz -lpthread 

$(DB2PF_TARGET): $(DB2PF_OBJ) m1rtlib
	$(CXX)  -L../../extern/lib  $(CXXFLAGS) -o $(DB2PF_TARGET) $(DB2PF_OBJ) $(M1_RTLIB) $(LDFLAGS) -lcrypto -lm -ldl -lmysqlclient -lz -lpthread 

$(DB2FS_TARGET): $(DB2FS_OBJ) m1rtlib
	$(CXX)  -L../../extern/lib  $(CXXFLAGS) -o $(DB2FS_TARGET) $(DB2FS_OBJ) $(M1_RTLIB) $(LDFLAGS) -lcrypto -lm -ldl -lmysqlclient -lz -lpthread 

$(FS2PF_TARGET): m1rtlib $(FS2PF_OBJ)
	$(CXX) $(CXXFLAGS) -o $(FS2PF_TARGET) $(FS2PF_OBJ) $(M1_RTLIB) $(LDFLAGS) -lcrypto -lm -ldl -lmysqlclient -lz -lpthread


$(PF2FS_TARGET): m1rtlib $(PF2FS_OBJ)
	$(CXX) $(CXXFLAGS) -o $(PF2FS_TARGET) $(PF2FS_OBJ) $(M1_RTLIB) $(LDFLAGS) -lcrypto -lm -ldl -lz 


$(PFLS_TARGET): m1rtlib $(PFLS_OBJ)
	$(CXX) $(CXXFLAGS) -o $(PFLS_TARGET) $(PFLS_OBJ) $(M1_RTLIB) $(LDFLAGS) -lcrypto -lm -ldl -lz 

$(PFINFO_TARGET): $(PFINFO_OBJ) 
	$(CXX) $(CXXFLAGS) -o $(PFINFO_TARGET) $(PFINFO_OBJ) 


%.o: %.cc pfcommon.hh ../../include/packfile.hh ../sqltool/sqlcommon.hh  ../../include/key.hh  ../../include/key_store.hh 
	$(CXX) -c $(CXXFLAGS) -o $@ $<

clean:
	rm -f $(FS2DB_TARGET) $(FS2DB_OBJ) $(DB2PF_TARGET) $(DB2FS_TARGET) $(DB2PF_OBJ) $(FS2PF_TARGET) $(FS2PF_OBJ) $(PF2FS_TARGET) $(PF2FS_OBJ) $(PFINFO_OBJ) $(PFINFO_TARGET) $(PFLS_OBJ) $(PFLS_TARGET)  sqlcommon.o

spotless: clean
	rm -rf config.status config.log configure autom4te.cache *~

sqlcommon.o: ../sqltool/sqlcommon.cc ../sqltool/sqlcommon.hh 
	$(CXX) -c $(CXXFLAGS) -DSTANDALONE -o sqlcommon.o ../sqltool/sqlcommon.cc

m1rtlib: 
	(cd ../../src; make rtlib)
