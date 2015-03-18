# Makefile: Rules to package, compile and install Chemlogic
# This file is from Chemlogic, a logic programming computer chemistry system  
# <http://icebergsystems.ca/chemlogic>  
# (C) Copyright 2012-2014 Nicholas Paun  

### This Makefile is written in GNU Make syntax ###



### Meta-Targets
.DEFAULT: all
all: cli web


# Recall make with variables set, based on user specified target
# This allows code to be re-used, while allowing the user to use targets
.PHONY: cli web web-daemon
cli web web-daemon: compile.cf
	$(MAKE) INTERFACE=$@ mk-$@


### Build Setup ###


# Set the default prefix
PREFIX ?= /usr/local


ifdef DEST
# If the user has explicitly specified a DEST, write all files there
BINDIR ?= $(DEST)
SHAREDIR ?= $(DEST)
else
# Otherwise, install to UNIX standard locations
	ifdef DESTDIR
		DESTDIR = $(DESTDIR)/
	endif

BINDIR ?= $(DESTDIR)$(PREFIX)/bin
SHAREDIR ?= $(DESTDIR)$(PREFIX)/share/chemlogic
endif

# Set the DEST for the output of the building process, if not set by user
# Files will be copied to installation location from here
DEST ?= bin/

# Set which Prolog system is used to compile Chemlogic.
PROLOG_SYSTEM ?= swipl
PROLOG_PATH_DETECT = $(shell which swipl)
PROLOG_PATH ?= $(PROLOG_PATH_DETECT)


### Help ###

help: 
	@echo "make <target>"
	@echo "	all: Build cli and web interfaces"
	@echo "	cli: Command-line interface"
	@echo "	web: Web interface"
	@echo "	web-daemon: Web interface as a UNIX Daemon"
	@echo "	install: Install the Chemlogic to correct paths"
	@echo "	dist: Make a tar.gz archive of the Chemlogic code"
	@echo ""
	@echo "Installation variables:"
	@echo "	PREFIX=<path> (default /usr/local) Prefix for installation directories (e.g. bin/, share/)"
	@echo " - or -"
	@echo "	DEST=<path> Install all Chemlogic files to the specified directory"

### Chemlogic Interfaces ###

mk-web mk-web-daemon: stage-style
mk-cli mk-web mk-web-daemon: compile-$(PROLOG_SYSTEM)

### Compilation and Building ###

compile-swipl:
	# Compiling using the SWI-Prolog QSAVE system.
	echo " \
cl_parse_all. \
qsave_program('$(DEST)/chem$(INTERFACE)'). \
" | $(PROLOG_PATH) -l $(INTERFACE)/chem$(INTERFACE).in


# Provide information to Prolog about the paths and other settings we have set up
.PHONY: compile.cf
compile.cf:
	#Clear everything in the file except the header
	cp build/compile.cf.dist build/compile.cf
	#Tell Prolog the prefix
	echo "cf_prefix('$(PREFIX)')." >> build/compile.cf	
	echo END


stage-style:
	# When building the Web interace, copy the style/ files to the DEST
	cp -aL $(INTERFACE)/style $(DEST)/

### Installation ###

install:
	mkdir -p $(BINDIR)
	cp -a bin/chem* $(BINDIR)/
	# If a style/ directory was produced (by building the Web Interface), copy these files
	if test -e bin/style; then \
		mkdir -p $(SHAREDIR) && \
		cp -a bin/style $(SHAREDIR)/; \
	fi

uninstall:
	-rm $(BINDIR)/chemcli
	-rm $(BINDIR)/chemweb
	-rm -r $(SHAREDIR)


### Creating Distributions ###

clean:
	# Build files
	-rm bin/chem*
	-rm -r bin/style/

	# Reset Compile options
	cp build/compile.cf.dist build/compile.cf


dist: archive

dist-tree: TAG = $(shell ./build/versionName)
dist-tree:
	cp -a ./ ../chemlogic-$(TAG)/
	./build/tagdist $(TAG)
	make2bsd ../chemlogic-$(TAG)/Makefile ../chemlogic-$(TAG)/BSDmakefile	
	rm -rf ../chemlogic-$(TAG)/.git/

archive: TAG = $(shell ./build/versionName)
archive: dist-tree
	tar -czf ../chemlogic-$(TAG).tar.gz ../chemlogic-$(TAG)/
