.ifmake install
DEST=/usr/local/mugu
.endif

all: cli web web-daemon
cli web web-daemon! 
	$(MAKE) IFACE=$(.TARGET) DEST=$(DEST) mk-$(.TARGET)
	
# Real rules

DEST?=bin/

mk-cli mk-web mk-web-daemon: qsave install-iface
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
	cd $(IFACE) ; \
	echo " \
cl_parse_all. \
qsave_program('chem$(IFACE).out'). \
" | swipl -l chem$(IFACE).in


install-iface:
	mv $(IFACE)/chem$(IFACE).out $(DEST)/chem$(IFACE)

install-style:
	cp -a $(IFACE)/style $(DEST)/
