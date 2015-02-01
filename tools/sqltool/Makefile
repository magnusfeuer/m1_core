#
# All rights reserved. Reproduction, modification, use or disclosure
# to third parties without express authority is forbidden.
# Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
#
#
# Makefile for sql tools forming the backend.
#

include ../../Version.inc

CXX := g++
# leftmost number corresponds to Format-Version number


BINDIR := ../../../bin
UNAME := $(shell uname)


ADDM1_TARGET := $(BINDIR)/add_m1
ADDM1_OBJ =  add_m1.o sqlcommon.o
ADDM1_FILES = $(ADDM1_OBJ) $(ADDM1_TARGET)

CGI_PRODUCTION_TARGET := $(BINDIR)/cgi_production
CGI_PRODUCTION_OBJ =  cgi_production.o sqlcommon.o cgicommon.o
CGI_PRODUCTION_FILES = $(CGI_PRODUCTION_OBJ) $(CGI_PRODUCTION_TARGET)

DB_RM_PF_TARGET := $(BINDIR)/db_rm_pf
DB_RM_PF_OBJ =  db_rm_pf.o sqlcommon.o
DB_RM_PF_FILES = $(DB_RM_PF_OBJ) $(DB_RM_PF_TARGET)

DB_LS_PF_TARGET := $(BINDIR)/db_ls_pf
DB_LS_PF_OBJ =  db_ls_pf.o sqlcommon.o
DB_LS_PF_FILES = $(DB_LS_PF_OBJ) $(DB_LS_PF_TARGET)

ADD_KEY_TARGET := $(BINDIR)/add_key
ADD_KEY_OBJ =  add_key.o sqlcommon.o
ADD_KEY_FILES = $(ADD_KEY_OBJ) $(ADD_KEY_TARGET)

GET_KEY_TARGET := $(BINDIR)/get_key
GET_KEY_OBJ =  get_key.o sqlcommon.o
GET_KEY_FILES = $(GET_KEY_OBJ) $(GET_KEY_TARGET)

GENSERIAL_TARGET := $(BINDIR)/gen_m1_serial
GENSERIAL_OBJ =  gen_m1_serial.o sqlcommon.o
GENSERIAL_FILES = $(GENSERIAL_OBJ) $(GENSERIAL_TARGET)

CGI_CREATE_ACCOUNT_TARGET := $(BINDIR)/cgi_create_account
CGI_CREATE_ACCOUNT_OBJ =  cgi_create_account.o sqlcommon.o cgicommon.o
CGI_CREATE_ACCOUNT_FILES = $(CGI_CREATE_ACCOUNT_OBJ) $(CGI_CREATE_ACCOUNT_TARGET)

CGI_CREATE_SALES_ORDER_TARGET := $(BINDIR)/cgi_create_sales_order
CGI_CREATE_SALES_ORDER_OBJ =  cgi_create_sales_order.o sqlcommon.o cgicommon.o
CGI_CREATE_SALES_ORDER_FILES = $(CGI_CREATE_SALES_ORDER_OBJ) $(CGI_CREATE_SALES_ORDER_TARGET)

CGI_LOGIN_TARGET := $(BINDIR)/cgi_login
CGI_LOGIN_OBJ =  cgi_login.o sqlcommon.o cgicommon.o
CGI_LOGIN_FILES = $(CGI_LOGIN_OBJ) $(CGI_LOGIN_TARGET)


CGI_EDIT_ACCOUNT_TARGET := $(BINDIR)/cgi_edit_account
CGI_EDIT_ACCOUNT_OBJ =  cgi_edit_account.o sqlcommon.o cgicommon.o
CGI_EDIT_ACCOUNT_FILES = $(CGI_EDIT_ACCOUNT_OBJ) $(CGI_EDIT_ACCOUNT_TARGET)



CGI_DISCOUNT_TARGET := $(BINDIR)/cgi_discount
CGI_DISCOUNT_OBJ =  cgi_discount.o sqlcommon.o cgicommon.o
CGI_DISCOUNT_FILES = $(CGI_DISCOUNT_OBJ) $(CGI_DISCOUNT_TARGET)


HTTP_POST_TARGET := $(BINDIR)/http_post
HTTP_POST_OBJ =  http_post.o
HTTP_POST_FILES = $(HTTP_POST_OBJ) $(HTTP_POST_TARGET)


#
# -static needed for linux since some of these apps will run from a bootable USB stick.
#
ifeq "$(UNAME)" "Darwin"
CXXFLAGS:= -g -DDARWIN=1 -DVERSION=\"$(VERSION)\"  -I../../include 
LDFLAGS += -L/opt/local/lib/mysql
else
CXXFLAGS:= -g  -DVERSION=\"$(VERSION)\"  -I../../include
LDFLAGS += -static
endif

all:  $(CGI_DISCOUNT_TARGET) $(CGI_CREATE_SALES_ORDER_TARGET) $(GET_KEY_TARGET) $(DB_LS_PF_TARGET)  $(CGI_PRODUCTION_TARGET) $(DB_RM_PF_TARGET) $(CGI_EDIT_ACCOUNT_TARGET) $(CGI_LOGIN_TARGET) $(CGI_CREATE_ACCOUNT_TARGET) $(GENSERIAL_TARGET) $(ADDM1_TARGET)  $(ADD_KEY_TARGET) # $(HTTP_POST_TARGET)

$(DB_LS_PF_TARGET): $(DB_LS_PF_OBJ) 
	$(CXX) $(CXXFLAGS) -o $(DB_LS_PF_TARGET) $(DB_LS_PF_OBJ) $(LDFLAGS) -lmysqlclient -ldl -lz -lpthread 

$(DB_RM_PF_TARGET): $(DB_RM_PF_OBJ) 
	$(CXX) $(CXXFLAGS) -o $(DB_RM_PF_TARGET) $(DB_RM_PF_OBJ) $(LDFLAGS) -lmysqlclient -ldl -lz -lpthread 

$(CGI_PRODUCTION_TARGET): $(CGI_PRODUCTION_OBJ) 
	$(CXX) $(CXXFLAGS) -o $(CGI_PRODUCTION_TARGET) $(CGI_PRODUCTION_OBJ) $(LDFLAGS) -lmysqlclient -ldl -lz -lpthread 

$(ADD_KEY_TARGET): $(ADD_KEY_OBJ) 
	$(CXX) $(CXXFLAGS) -o $(ADD_KEY_TARGET) $(ADD_KEY_OBJ) $(LDFLAGS) -lmysqlclient -ldl -lz -lpthread 

$(GET_KEY_TARGET): $(GET_KEY_OBJ) 
	$(CXX) $(CXXFLAGS) -o $(GET_KEY_TARGET) $(GET_KEY_OBJ) $(LDFLAGS) -lmysqlclient -ldl -lz -lpthread 

$(HTTP_POST_TARGET): $(HTTP_POST_OBJ) 
	$(CXX) $(CXXFLAGS) -o $(HTTP_POST_TARGET) $(HTTP_POST_OBJ) $(LDFLAGS) -lcurl -lssl

$(ADDM1_TARGET): $(ADDM1_OBJ) 
	$(CXX) $(CXXFLAGS) -o $(ADDM1_TARGET) $(ADDM1_OBJ)  $(LDFLAGS) -lmysqlclient -ldl -lz -lpthread 

$(GENSERIAL_TARGET): $(GENSERIAL_OBJ) 
	$(CXX) $(CXXFLAGS) -o $(GENSERIAL_TARGET) $(GENSERIAL_OBJ)  $(LDFLAGS)  -lmysqlclient -ldl -lz -lpthread 

$(CGI_CREATE_ACCOUNT_TARGET): $(CGI_CREATE_ACCOUNT_OBJ) 
	$(CXX) $(CXXFLAGS) -o $(CGI_CREATE_ACCOUNT_TARGET) $(CGI_CREATE_ACCOUNT_OBJ)  $(LDFLAGS)  -lmysqlclient -ldl -lz -lpthread 

$(CGI_DISCOUNT_TARGET): $(CGI_DISCOUNT_OBJ) 
	$(CXX) $(CXXFLAGS) -o $(CGI_DISCOUNT_TARGET) $(CGI_DISCOUNT_OBJ)  $(LDFLAGS)  -lmysqlclient -ldl -lz -lpthread 

$(CGI_CREATE_SALES_ORDER_TARGET): $(CGI_CREATE_SALES_ORDER_OBJ) 
	$(CXX) $(CXXFLAGS) -o $(CGI_CREATE_SALES_ORDER_TARGET) $(CGI_CREATE_SALES_ORDER_OBJ)  $(LDFLAGS)  -lmysqlclient -ldl -lz -lpthread 

$(CGI_EDIT_ACCOUNT_TARGET): $(CGI_EDIT_ACCOUNT_OBJ) 
	$(CXX) $(CXXFLAGS) -o $(CGI_EDIT_ACCOUNT_TARGET) $(CGI_EDIT_ACCOUNT_OBJ)  $(LDFLAGS)  -lmysqlclient -ldl -lz -lpthread

$(CGI_LOGIN_TARGET): $(CGI_LOGIN_OBJ) 
	$(CXX) $(CXXFLAGS) -o $(CGI_LOGIN_TARGET) $(CGI_LOGIN_OBJ)  $(LDFLAGS)  -lmysqlclient -ldl -lz -lpthread


%.o: %.cc cgicommon.hh sqlcommon.hh
	$(CXX) -c $(CXXFLAGS) -o $@ $<

clean:
	rm -f $(CGI_CREATE_SALES_ORDER_FILES) $(GET_KEY_FILES) $(DB_LS_PF_FILES) $(DB_RM_PF_FILES) $(CGI_PRODUCTION_FILES) $(ADD_KEY_FILES) $(CGI_EDIT_ACCOUNT_FILES) $(CGI_CREATE_ACCOUNT_FILES) $(CGI_DISCOUNT_FILES) $(ADDM1_FILES) $(GENSERIAL_FILES) $(CGI_LOGIN_FILES) \
	packfile.o sqlcommon.o cgicommon.o

spotless: clean
	rm -rf config.status config.log configure autom4te.cache *~

packfile.o: ../../src/packfile.cc ../../include/packfile.hh
	$(CXX) -c $(CXXFLAGS) -DSTANDALONE -o packfile.o ../../src/packfile.cc

