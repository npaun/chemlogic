#!/bin/sh -x
# build.sh: A simple bash script to compile Chemlogic interfaces with qsave
# This file is from Chemlogic, a logic programming computer chemistry system
# <http://icebergsystems.ca/chemlogic>
# (C) Copyright 2012-2014 Nicholas Paun



IFACE=$1
PROGNAME=chem$IFACE
DEST=$2




if [ -z "$IFACE" ]; then
	echo "$0 <interface> <dest>: Compiles a Chemlogic interface to destination"
	echo "Interface is either cli or web, or use all to make both interfaces"
	exit
fi

if [ $IFACE == 'all' ]; then
	$0 cli $2
	$0 web $2
	exit
fi

if [ -z "$DEST" ]; then
	DEST='../bin/'
fi


cd $IFACE

if [ $IFACE == 'web-daemon' ]; then
	swipl -g 'http_daemon([port(8000),user(www)])' -o $$.build -c $PROGNAME\.in
else
	echo "
cl_parse_all.
qsave_program('$$.build').
" | swipl -l $PROGNAME\.in
fi

mv $$.build $DEST/$PROGNAME

if [ $IFACE == 'web' ] || [ $IFACE == 'web-daemon' ]; then
	cp -aL style $DEST
fi


echo "* Built	$IFACE,	Copied to	$DEST"
