#!/bin/sh
# build.sh: A simple bash script to compile Chemlogic interfaces with qsave
# This file is from Chemlogic, a logic programming computer chemistry system
# <http://icebergsystems.ca/chemlogic>
# (C) Copyright 2012-2014 Nicholas Paun



TASK=$1
PROGNAME=chem$TASK
DEST=$2




if [ -z "$TASK" ]; then
	echo "$0 <interface> <dest>: Compiles a Chemlogic interface to destination"
	echo "$0 dist: Create a Chemlogic package"
	echo "Interface is either cli or web, or use all to make both interfaces"
	echo "The web interface can also be compiled as a daemon (<interface> is web-daemon)"
	exit
fi

if [ $TASK == 'dist' ]; then
	TAG=`git tag -l | tail -1`
	BUILDER="$USER@`hostname`"
	DATE=`date "+%F %R"`
	COMMIT=`git log --pretty=format:%H HEAD -1`
	SOURCEPATH=$PWD

	cp -av ./ ../chemlogic-$TAG/
	cd ../chemlogic-$TAG/
	rm -rf  .git/ .repo/
	echo "
---
Chemlogic $TAG 
($COMMIT)
---

<$BUILDER>
Built at: $DATE
Source path: $SOURCEPATH" > Buildinfo

	cd ..

	tar -czvf chemlogic-$TAG\.tar.gz chemlogic-$TAG
	exit
fi

if [ $TASK == 'all' ]; then
	$0 cli $2
	$0 web $2
	exit
fi

if [ -z "$DEST" ]; then
	DEST='../bin/'
fi


cd $TASK

if [ $TASK == 'web-daemon' ]; then
	swipl -g 'http_daemon([port(8000),user(www)])' -o $$.build -c $PROGNAME\.in
else
	echo "
cl_parse_all.
qsave_program('$$.build').
" | swipl -l $PROGNAME\.in
fi

mv $$.build $DEST/$PROGNAME

if [ $TASK == 'web' ] || [ $TASK == 'web-daemon' ]; then
	cp -a style $DEST
fi


echo "* Built	$TASK,	Copied to	$DEST"
