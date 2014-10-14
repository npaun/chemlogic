all: cli web
cli web web-daemon! 
	$(MAKE) INTERFACE=$(.TARGET) DEST=$(DEST) mk-$(.TARGET)

help: 
	@echo "make <target> [DEST=<destination>]"
	@echo "	all: Build cli and web interfaces"
	@echo "	cli: Command-line interface"
	@echo "	web: Web interface"
	@echo "	web-daemon: Web interface as a UNIX Daemon"
	@echo "	install: Install any of the previous selections into /usr/local/bin"
	@echo "	dist: Make a tar.gz archive of the Chemlogic code"

# Real rules



PREFIX?=/usr/local

.ifdef DEST
BINDIR?=$(DEST)
SHAREDIR?=$(DEST)
.else
BINDIR?=$(PREFIX)/bin
SHAREDIR?=$(PREFIX)/share/chemlogic
.endif

DEST ?= bin/

install:
	cp -a bin/chem* $(BINDIR)/
.if exists(bin/style)
	mkdir -p $(SHAREDIR)
	cp -a bin/style $(SHAREDIR)/
.endif

uninstall:
	-rm $(BINDIR)/chemcli
	-rm $(BINDIR)/chemweb
	-rm -r $(SHAREDIR)

mk-cli mk-web mk-web-daemon: qsave
mk-web mk-web-daemon: stage-style

dist: clean archive

clean:
	# Build files
	-rm bin/chem*
	-rm -r bin/style/

.ifmake dist || disttree || archive
TAG != git tag -l | tail -1
.endif

disttree:
	echo $(TAG)
	cp -av ./ ../chemlogic-$(TAG)/
	cd ../chemlogic-$(TAG)/; \
	rm -rf  .git/ .repo/ ; \
	$(PWD)/tagbuild $(PWD)

archive: disttree
	cd ..; \
	tar -czvf chemlogic-$(TAG).tar.gz chemlogic-$(TAG)
 

compile.cf!
	sed -i '' '1,10!d' compile.cf
	echo "prefix('$(PREFIX)')." >> compile.cf	


qsave: compile.cf
	echo " \
cl_parse_all. \
qsave_program('$(DEST)/chem$(INTERFACE)'). \
" | swipl -l $(INTERFACE)/chem$(INTERFACE).in



stage-style:
	cp -a $(INTERFACE)/style $(DEST)/
