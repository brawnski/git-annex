all: git-annex docs

git-annex:
	mkdir -p build
	ghc -odir build -hidir build --make git-annex

install:
	install -d $(DESTDIR)/usr/bin
	install git-annex $(DESTDIR)/usr/bin

clean:
	rm -rf build git-annex
	rm -rf doc/.ikiwiki html

# If ikiwiki is available, build static html docs suitable for being
# shipped in the software package.
ifeq ($(shell which ikiwiki),)
IKIWIKI=echo "** ikiwiki not found, skipping building docs" >&2
else
IKIWIKI=ikiwiki
endif

docs:
	$(IKIWIKI) doc html -v --wikiname git-annex --plugin=goodstuff \
		--no-usedirs

.PHONY: git-annex
