#!/bin/bash

XMONAD_DIR=$1
XMONAD_DIR_BACKUP=./.xmonad.backup

XMONAD_GIT=./xmonad-git
XMONAD_CONTRIB_GIT=./xmonad-contrib-git
XMOBAR_GIT=./xmobar-git

#-----------------
# install stack
#

./install_stack.sh

#-----------------
# Configure .xmonad directory location
#
if [ ${XMONAD_DIR} == "" ];
then
  XMONAD_DIR=~/.xmonad;
fi;

#-----------------
# backup .xmonad directory 


if [ -d ${XMONAD_DIR} ];
then
  mv ${XMONAD_DIR} ${XMONAD_DIR_BACKUP}
fi
mkdir ${XMONAD_DIR};

#-----------------
# copy xmonad, xmobar, and dependencies
#
echo "copying xmonad -> .xmonad/";
cp -r ${XMONAD_GIT} ${XMONAD_DIR}/;

echo "copying xmonad-contrib -> .xmonad/"
cp -r ${XMONAD_CONTRIB_GIT} ${XMONAD_DIR}/

echo "copying xmobar -> .xmonad/"
cp -r ${XMOBAR_GIT} ${XMONAD_DIR}/

#-----------------
# install GHC into ~/.stack
#
stack steup;

#-----------------
# setup stack project
#
# install into ~/.local/bin
#
cd ${XMONAD_DIR};
stack init;
stack install;

#-----------------
# copy build file
#
cp ./build ${XMONAD_DIR}/build
chmod a+x ${XMONAD_DIR}/build


#-----------------
# copy configuration files
#

if [ -d ${XMONAD_DIR_BACKUP} ];
then
  cp ${XMONAD_DIR_BACKUP}/xmonad.hs ${XMONAD_DIR}/xmonad.hs
fi;

if [ -d ${XMOBAR_DIR} ];
then
  cp ${XMOBAR_DIR}/* ${XMOBAR_CONTRIB_GIT}/
fi;

echo "Installation successful!"

#=================================================================
