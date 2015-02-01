AC_INIT(src/m1e_main.cc)


PACKAGE="m1"
TARGET="m1"

dnl How to set srcdir absolute is taken from the GNU Emacs distribution
#### Make srcdir absolute, if it isn't already.  It's important to
#### avoid running the path through pwd unnecessary, since pwd can
#### give you automounter prefixes, which can go away.
case "${srcdir}" in
  /* ) ;;
  . )
    ## We may be able to use the $PWD environment variable to make this
    ## absolute.  But sometimes PWD is inaccurate.
    ## Make sure CDPATH doesn't affect cd (in case PWD is relative).
    CDPATH=
    if test "${PWD}" != "" && test "`(cd ${PWD} ; sh -c pwd)`" = "`pwd`"  ;
    then
      srcdir="$PWD"
    else
      srcdir="`(cd ${srcdir}; pwd)`"
    fi
  ;;
  *  ) srcdir="`(cd ${srcdir}; pwd)`" ;;
esac


AC_SUBST(Target, $TARGET)

AC_PROG_MAKE_SET
AC_HEADER_STDC
AC_PROG_CXX
AC_PROG_CPP
AC_PROG_RANLIB
AC_PROG_YACC
AC_PROG_LEX
AC_PROG_EGREP
EGREP=/usr/bin/egrep


AC_CHECK_LIB(dl, dlopen)

AC_SUBST(USE_FFMPEG)
AC_SUBST(USE_SWSCALE)
AC_SUBST(FFMPEG_LIB)
AC_SUBST(FFMPEG_INCLUDE)
AC_SUBST(FFMPEG_AVCODEC_LIB)
AC_SUBST(FFMPEG_SWSCALE_LIB)

AC_CONFIG_HEADER($srcdir/include/config.h:config.h.in)

AC_ARG_WITH(ffmpeg,
[  --with-ffmpeg=PATH      speficfy location of ffmpeg
  --with-ffmpeg           use ffmpeg (default)
  --without-ffmpeg	  don't use ffmpeg
])


ffmpeg_found=no
ffmpeg_dir=

if test "x$with_ffmpeg" = "xno"; then
   FFMPEG_LIB=
   FFMPEG_INCLUDE=
   FFMPEG_AVCODEC_LIB=
   FFMPEG_SWSCALE_LIB=
elif test "x$with_ffmpeg" = "xyes" -o "x$with_ffmpeg" = "x"; then
    AC_MSG_CHECKING([for ffmpeg in build & standard locations])
    for dir in $srcdir/extern /usr/local /opt/local /usr;
    do
    	if test -f $dir/include/ffmpeg/avcodec.h; then
	   FFMPEG_LIB=-L$dir/lib
	   FFMPEG_INCLUDE=-I$dir/include
	   ffmpeg_dir=$dir
	   ffmpeg_found=yes
	   break
	fi
    done
else
    AC_MSG_CHECKING([for ffmpeg in $with_ffmpeg])
    dir=$with_ffmpeg
    if test -f $dir/include/ffmpeg/avcodec.h; then
       ffmpeg_dir=$dir
       FFMPEG_LIB=-L$dir/lib
       FFMPEG_INCLUDE=-I$dir/include
    fi
fi

if test "x$with_ffmpeg" != "xno"; then
   AC_MSG_RESULT([$ffmpeg_found])
   if test "x$ffmpeg_found" != "xyes"; then
      AC_MSG_RESULT([no])
      FFMPEG_LIB=
      FFMPEG_INCLUDE=
      FFMPEG_AVCODEC_LIB=
      FFMPEG_SWSCALE_LIB=
   else
      FFMPEG_AVCODEC_LIB="-lavformat -lavcodec -lavutil -lz"
      if test -f $ffmpeg_dir/include/ffmpeg/swscale.h; then
      	 FFMPEG_SWSCALE_LIB="-lswscale"
	 AC_DEFINE(USE_SWSCALE)
      fi
      AC_DEFINE(USE_FFMPEG)
   fi
fi

AC_OUTPUT(src/Makefile)
