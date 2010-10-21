all: git-annex docs

git-annex:
	mkdir -p build
	ghc -odir build -hidir build --make git-annex

install:
	install -d $(DESTDIR)/usr/bin
	install git-annex $(DESTDIR)/usr/bin

# If ikiwiki is available, build static html docs suitable for being
# shipped in the software package.
ifeq ($(shell which ikiwiki),)
IKIWIKI=echo "** ikiwiki not found, skipping building docs" >&2
else
IKIWIKI=ikiwiki
endif

docs:
	./mdwn2man git-annex 1 doc/git-annex.mdwn > git-annex.1
	$(IKIWIKI) doc html -v --wikiname git-annex --plugin=goodstuff \
		--no-usedirs --disable-plugin=openid --plugin=sidebar \
		--underlaydir=/dev/null --disable-plugin=shortcut

clean:
	rm -rf build git-annex git-annex.1
	rm -rf doc/.ikiwiki html

.PHONY: git-annex
