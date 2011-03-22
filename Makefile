PREFIX=/usr
GHCFLAGS=-O2 -Wall -ignore-package monads-fd
GHCMAKE=ghc $(GHCFLAGS) --make

bins=git-annex git-annex-shell
mans=git-annex.1 git-annex-shell.1

all: $(bins) $(mans) docs

SysConfig.hs: configure.hs TestConfig.hs
	$(GHCMAKE) configure
	./configure

%.hs: %.hsc
	hsc2hs $<
	perl -i -pe 's/^{-# INCLUDE.*//' $@

$(bins): SysConfig.hs Touch.hs StatFS.hs
	$(GHCMAKE) $@

git-annex.1: doc/git-annex.mdwn
	./mdwn2man git-annex 1 doc/git-annex.mdwn > git-annex.1
git-annex-shell.1: doc/git-annex-shell.mdwn
	./mdwn2man git-annex-shell 1 doc/git-annex-shell.mdwn > git-annex-shell.1

install: all
	install -d $(DESTDIR)$(PREFIX)/bin
	install $(bins) $(DESTDIR)$(PREFIX)/bin
	install -d $(DESTDIR)$(PREFIX)/share/man/man1
	install -m 0644 $(mans) $(DESTDIR)$(PREFIX)/share/man/man1
	install -d $(DESTDIR)$(PREFIX)/share/doc/git-annex
	if [ -d html ]; then \
		rsync -a --delete html/ $(DESTDIR)$(PREFIX)/share/doc/git-annex/html/; \
	fi

test: $(bins)
	$(GHCMAKE) test
	./test

testcoverage: $(bins)
	rm -f test.tix test
	ghc -odir build/test -hidir build/test $(GHCFLAGS) --make -fhpc test
	./test
	@echo ""
	@hpc report test --exclude=Main --exclude=QC
	@hpc markup test --exclude=Main --exclude=QC --destdir=.hpc >/dev/null

# If ikiwiki is available, build static html docs suitable for being
# shipped in the software package.
ifeq ($(shell which ikiwiki),)
IKIWIKI=@echo "** ikiwiki not found, skipping building docs" >&2; true
else
IKIWIKI=ikiwiki
endif

docs: $(mans)
	$(IKIWIKI) doc html -v --wikiname git-annex --plugin=goodstuff \
		--no-usedirs --disable-plugin=openid --plugin=sidebar \
		--underlaydir=/dev/null --disable-plugin=shortcut \
		--disable-plugin=smiley \
		--exclude='news/.*'

clean:
	rm -rf build $(bins) $(mans) test configure Touch.hs SysConfig.hs *.tix .hpc
	rm -rf doc/.ikiwiki html
	find . \( -name \*.o -or -name \*.hi \) -exec rm {} \;

.PHONY: $(bins) test install
