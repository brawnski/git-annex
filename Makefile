git-annex:
	mkdir -p build
	ghc -odir build -hidir build --make git-annex

install:
	install -d $(DESTDIR)/usr/bin
	install git-annex $(DESTDIR)/usr/bin

clean:
	rm -rf build git-annex
	rm -rf doc/.ikiwiki html

# Build static html docs suitable for being shipped in the software
# package. This depends on ikiwiki being installed to build the docs.
ifeq ($(shell which ikiwiki),)
IKIWIKI=echo "** ikiwiki not found" >&2 ; echo ikiwiki
else
IKIWIKI=ikiwiki
endif

docs:
	$(IKIWIKI) doc html -v --wikiname git-annex --plugin=goodstuff

.PHONY: git-annex
