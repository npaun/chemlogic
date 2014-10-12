.ifmake install
DEST?=/usr/local/mugu
.endif

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

DEST?=bin/

mk-cli mk-web mk-web-daemon: qsave install-interface
mk-web mk-web-daemon: install-style

dist: clean package archive

clean:
	# Build files
	-rm */*.out
	-rm bin/chem*
	-rm -r bin/style/

.ifmake dist || package || archive
TAG != git tag -l | tail -1
.endif

package:
	echo $(TAG)
	cp -av ./ ../chemlogic-$(TAG)/
	cd ../chemlogic-$(TAG)/; \
	rm -rf  .git/ .repo/ ; \
	$(PWD)/tagbuild $(PWD)

archive:
	cd ..; \
	tar -czvf chemlogic-$(TAG).tar.gz chemlogic-$(TAG)
 

qsave:
	cd $(INTERFACE) ; \
	echo " \
cl_parse_all. \
qsave_program('chem$(INTERFACE).out'). \
" | swipl -l chem$(INTERFACE).in


install-interface:
	mv $(INTERFACE)/chem$(INTERFACE).out $(DEST)/chem$(INTERFACE)

install-style:
	cp -a $(INTERFACE)/style $(DEST)/
