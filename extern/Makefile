#
# Checkout trigger for ffmpeg and openssl
#
OSNAME := $(shell uname -s)

.PHONY: all spline2

#all: lib/libavcodec.a lib/libssl.a  spline2 bin/bison
all:  spline2 bin/bison lib/libavcodec.a

spline2:
	(cd spline2; make)

bin/bison: bison-2.3/Makefile
	@echo "--- extern: Building bison."
	(cd bison-2.3; make; make install)
# Install specific m1 stuff in share/bison
	rm -f ./share/bison/lalr1pp.cc
	ln ../src/lalr1pp.cc ./share/bison/

lib/libavcodec.a: ffmpeg/Makefile
	@echo "--- extern: Building ffmpeg."
	(cd ffmpeg; make; make install)

bison-2.3/Makefile: bison-2.3/README
	@echo "--- extern: Confiuring bison."
	(cd bison-2.3; ./configure --prefix=$(PWD))

bison-2.3/README:
	@echo "--- extern: Unpacking bison 2.3."
	tar xzf  bison-2.3.tar.gz

ffmpeg/Makefile: ffmpeg/INSTALL
	@echo "--- extern: Configuring ffmpeg."
	(cd ffmpeg; ./configure --enable-swscaler --enable-gpl --prefix=$(PWD); make depend);
	-(cd ffmpeg; make depend);
#	(cd ffmpeg; ./configure  --prefix=$(PWD));

ffmpeg/INSTALL:
	@echo "--- extern: Unpacking ffmpeg from local 2006-12-01 snapshot."
	tar xzf ffmpeg_20070413.tgz

lib/libssl.a: openssl-0.9.8e/Makefile.bak
	@echo "--- extern: Building OpenSSL."
	(cd openssl-0.9.8e; make all install_sw)

openssl-0.9.8e/Makefile.bak: openssl-0.9.8e/README
	@echo "--- extern: Configuring OpenSSL."
	(cd openssl-0.9.8e; ./config  --prefix=$(PWD) zlib);

openssl-0.9.8e/README:
	@echo "--- extern: Unpacking OpenSSL."
	tar xzf openssl-0.9.8e.tar.gz
ifeq ($(OSNAME), Darwin)
	(cd openssl-0.9.8e; patch < ../openssl-0.9.8e-mac_os_x.patch)
endif

clean:
	rm -rf lib include ssl bin ffmpeg openssl-0.9.8e bison-2.3
