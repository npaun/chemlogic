### Meta-Targets

all: cli web


# Recall make with variables set, based on user specified target
# This allows code to be re-used, while allowing the user to use targets
cli web web-daemon! 
	$(MAKE) INTERFACE=$(.TARGET) DEST=$(DEST) mk-$(.TARGET)


### Build Setup ###


# Set the default prefix
PREFIX?=/usr/local

.ifdef DEST
# If the user has explicitly specified a DEST, write all files there
BINDIR?=$(DEST)
SHAREDIR?=$(DEST)
.else
# Otherwise, install to UNIX standard locations
BINDIR?=$(PREFIX)/bin
SHAREDIR?=$(PREFIX)/share/chemlogic
.endif

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



qsave: compile.cf
	# This compiles the program using SWI-Prolog's QSAVE system
	echo " \
cl_parse_all. \
qsave_program('$(DEST)/chem$(INTERFACE)'). \
" | swipl -l $(INTERFACE)/chem$(INTERFACE).in

# Provide information to Prolog about the paths and other settings we have set up
compile.cf!
	#Clear everything in the file except the header
	sed -i '' '1,10!d' compile.cf
	#Tell Prolog the prefix
	echo "prefix('$(PREFIX)')." >> compile.cf	



stage-style:
	# When building the Web interace, copy the style/ files to the DEST
	cp -a $(INTERFACE)/style $(DEST)/

### Installation ###


install:
	cp -a bin/chem* $(BINDIR)/
.if exists(bin/style)
	# If a style/ directory was produced (by building the Web Interface), copy these files
	mkdir -p $(SHAREDIR)
	cp -a bin/style $(SHAREDIR)/
.endif


uninstall:
	-rm $(BINDIR)/chemcli
	-rm $(BINDIR)/chemweb
	-rm -r $(SHAREDIR)


### Creating Distributions ###


dist: clean archive

clean:
	# Build files
	-rm bin/chem*
	-rm -r bin/style/

	#Clear everything in the file except the header
	sed -i '' '1,10!d' compile.cf

.ifmake dist || disttree || archive
# Read tag from git
TAG != git tag -l | tail -1
.endif

disttree:
	echo $(TAG)
	# Make a copy of the Chemlogic source tree
	cp -av ./ ../chemlogic-$(TAG)/
	cd ../chemlogic-$(TAG)/; \
	# Remove the .git repository and repository scripts	
	rm -rf  .git/ .repo/ ; \
	# Place a file in the new tree with information about the Git commits so that I can figure out how I produced a certain distribution. 	
	$(PWD)/tagbuild $(PWD)

archive: disttree
	cd ..; \
	tar -czvf chemlogic-$(TAG).tar.gz chemlogic-$(TAG)
 
