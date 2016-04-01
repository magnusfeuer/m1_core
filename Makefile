#
# Top level makefile
#
all: debug 
# all: release debug drm 

include Version.inc

BINDIR:=$(PWD)/../bin
EPX_CLONE_TRIGGER=./epx/.git/HEAD

#
# Build core m1 distro with no skins
#
pf: 
	@-rm -rf ptmp
	@-mkdir -p ptmp/m1
	@-mkdir ptmp/m1/dds
	@-mkdir ptmp/m1/plugin
	@-mkdir ptmp/m1/launch
	@-mkdir ptmp/m1/keys
	@-mkdir ptmp/m1/m1
	@ln $(BINDIR)/m1e ptmp/m1/m1e  # NO DRM PROTECTION!
	@ln $(BINDIR)/spline2 ptmp/m1/spline2
	@ln $(BINDIR)/hpcurve ptmp/m1/hpcurve
	@ln $(BINDIR)/evalsp ptmp/m1/evalsp
	@(cd ptmp; $(BINDIR)/fs2pf -o ../../packfiles/m1_bin.pfl -i m1@magden-auto.com/m1_bin/$(VERSION) -D m1@magden-auto.com/os/1.-1.-1 .)
	@rm -rf ptmp

dpf: 
	@-rm -rf ptmp
	@-mkdir -p ptmp/m1
	@-mkdir ptmp/m1/dds
	@-mkdir ptmp/m1/plugin
	@-mkdir ptmp/m1/launch
	@-mkdir ptmp/m1/keys
	@-mkdir ptmp/m1/m1
	@ln $(BINDIR)/m1e.drm ptmp/m1/m1e
	@ln $(BINDIR)/spline2 ptmp/m1/spline2
	@ln $(BINDIR)/hpcurve ptmp/m1/hpcurve
	@ln $(BINDIR)/evalsp ptmp/m1/evalsp
	@(rev=`svn info | grep Revision | cut -d' ' -f 2`; cd ptmp; $(BINDIR)/fs2db -h$(DB_HOST) -u$(DB_USER) -p$(DB_PASSWORD) -d$(DB_DATABASE) -s$$rev -Dm1@magden-auto.com/os/1.-1.-1 -L -im1@magden-auto.com/m1_bin/$(VERSION) -as . )
	@rm -rf ptmp

debug_dpf: 
	@-rm -rf ptmp
	@-mkdir -p ptmp/m1
	@-mkdir ptmp/m1/dds
	@-mkdir ptmp/m1/plugin
	@-mkdir ptmp/m1/launch
	@-mkdir ptmp/m1/keys
	@-mkdir ptmp/m1/m1
	@ln $(BINDIR)/m1e.debug ptmp/m1/m1e
	@ln $(BINDIR)/spline2 ptmp/m1/spline2
	@ln $(BINDIR)/hpcurve ptmp/m1/hpcurve
	@ln $(BINDIR)/evalsp ptmp/m1/evalsp
	@-$(BINDIR)/db_rm_pf -i m1@magden-auto.com/m1dbg_bin/%
	@(rev=`svn info | grep Revision | cut -d' ' -f 2`; cd ptmp; $(BINDIR)/fs2db -h$(DB_HOST) -u$(DB_USER) -p$(DB_PASSWORD) -d$(DB_DATABASE) -s$$rev -Dm1@magden-auto.com/os/1.-1.-1 -L -im1@magden-auto.com/m1dbg_bin/$(VERSION) -as . )
	@rm -rf ptmp

release: src/Makefile
	(cd extern; make)
	(cd src; make rtlib)
#	(rm -f epic/obj/release/epic_backend.o; cd epic/src; make release)
	(cd src; make release)
	(cd tools; make)


drm: src/Makefile
	(cd extern; make)
#	(rm -f epic/obj/release/epic_backend.o; cd epic/src; make release)
	(cd src; make drm)
	(cd tools; make)

release_with_x11:
	(cd extern; make)
	(cd src; make rtlib)
#	(rm -f epic/obj/release/epic_backend.o; cd epic/src; make release WITH_X11=1)
	(cd src; make release WITH_X11=1)
	(cd tools; make)

debug:  src/Makefile $(EPX_CLONE_TRIGGER)
	(cd extern; make)
	(cd src; make rtlib)
#	(rm -f epic/obj/debug/epic_backend.o; cd epic/src; make debug WITH_X11=1)
	(cd src; make debug WITH_X11=1)
	(cd tools; make)

debug_no_x11: src/Makefile $(EPX_CLONE_TRIGGER)
	(cd extern; make)
	(cd src; make rtlib)
#	(rm -f epic/obj/debug/epic_backend.o; cd epic/src; make debug)
	(cd src; make debug)
	(cd tools; make)

clean:
	-(cd tools; make clean)
	-(cd extern; make clean)
	-(cd src; make clean)
#	-(cd epic/src; make clean clean_debug)

light_clean:
	-(cd src; make clean)
#	-(cd epic/src; make clean clean_debug)
	-(cd tools; make clean)

doc:	FORCE
	doxygen doxygen.cfg

m1doc:	FORCE
	../../../bin/m1_typeinfo c++ > m1_doc/m1_types.hh
	doxygen doxygen_m1.cfg

src/Makefile: src/Makefile.in configure
	./config.status

configure: configure.ac
	autoconf;
	./configure --with-ffmpeg=$$PWD/extern

FORCE:

spotless: clean
	(cd epx; make clean)
	rm -rf autom4te.cache config.status configure config.log src/Makefile


$(EPX_CLONE_TRIGGER):
	@echo
	@echo "--- Checking out epx."
	git clone -b $(EPX_GIT_BRANCH) $(EPX_GIT_REPO) epx

