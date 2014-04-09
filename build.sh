#!/bin/sh

IFACE=$1
PROGNAME=chem$IFACE
DEST=$2

if [ -z "$IFACE" ]; then
	echo "$0 <interface> <dest>: Compiles a Chemlogic interface to destination"
	echo "Interface is either cli or web, or use all to make both interfaces"
	exit
fi

if [ $IFACE == 'all' ]; then
	$0 cli
	$0 web
fi

if [ -z "$DEST" ]; then
	DEST='../bin/'
fi


cd $IFACE
echo "
cl_parse_all.
qsave_program('$DEST/$PROGNAME').
" | swipl -l $PROGNAME\.in

echo "* Built	$IFACE,	Copied to	$DEST"


if [ $IFACE == 'web' ]; then
	cp -a style $DEST
fi
