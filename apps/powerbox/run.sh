#!/bin/sh
#
#
export M1_PATH=`cd ../../../../..; pwd`
export APPS=$M1_PATH/m1/core/trunk/apps
export M1_DATABASE=/tmp/m1.db
export M1_LIB_PATH=$M1_PATH/out/plugin
export M1_FONT_PATH=$M1_PATH/out/dds
(cd $APPS/powerbox/src; $M1_PATH/bin/m1e slider.m1 powerbox.m1 powerbox_control.m1)
