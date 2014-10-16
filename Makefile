# Makefile
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
	$(MAKE) INTERFACE=$@ DEST=$(DEST) mk-$@


### Build Setup ###


# Set the default prefix
PREFIX?=/usr/local

ifdef DEST
# If the user has explicitly specified a DEST, write all files there
BINDIR?=$(DEST)
SHAREDIR?=$(DEST)
else
# Otherwise, install to UNIX standard locations
BINDIR?=$(PREFIX)/bin
SHAREDIR?=$(PREFIX)/share/chemlogic
endif

# Set the DEST for the output of the building process, if not set by user
# Files will be copied to installation location from here
DEST ?= bin/


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

mk-cli mk-web mk-web-daemon: qsave	
mk-web mk-web-daemon: stage-style

### Compilation and Building ###



qsave:
	# This compiles the program using SWI-Prolog's QSAVE system
	echo " \
cl_parse_all. \
qsave_program('$(DEST)/chem$(INTERFACE)'). \
" | swipl -l $(INTERFACE)/chem$(INTERFACE).in

# Provide information to Prolog about the paths and other settings we have set up
.PHONY: compile.cf
compile.cf:
	#Clear everything in the file except the header
	cp compile.cf.dist compile.cf
	#Tell Prolog the prefix
	echo "prefix('$(PREFIX)')." >> compile.cf	
	echo END


stage-style:
	# When building the Web interace, copy the style/ files to the DEST
	cp -a $(INTERFACE)/style $(DEST)/

### Installation ###

install:
	cp -a bin/chem* $(BINDIR)/
	# If a style/ directory was produced (by building the Web Interface), copy these files
	test -e bin/style &&\
		mkdir -p $(SHAREDIR) &&\
       		cp -a bin/style $(SHAREDIR)/


uninstall:
	-rm $(BINDIR)/chemcli
	-rm $(BINDIR)/chemweb
	-rm -r $(SHAREDIR)


### Creating Distributions ###

# BSD: .ifmake dist || disttree || archive
# BSD: TAG != git tag | tail -1
# BSD: .endif

dist disttree archive: TAG := $(strip $(shell git tag | tail -1))#<<< GNU

#dist: archive

clean:
	# Build files
	-rm bin/chem*
	-rm -r bin/style/

	# Reset Compile options
	cp compile.cf.dist compile.cf

dist: clean
	cp -av ./ ../chemlogic-$(TAG)
	$(MAKE) -C ../chemlogic-$(TAG) archive

disttree:
	# Place a file in the new tree with information about the Git commits so that I can figure out how I produced a certain distribution. 	
	$(PWD)/tagbuild $(PWD)
	rm -rf  .git/ .gitignore .repo/

archive: disttree
	cd ../; tar -czvf chemlogic-$(TAG).tar.gz chemlogic-$(TAG)
 
