PREFIX=/usr
GHCFLAGS=-O2 -Wall
GHCMAKE=ghc -odir build -hidir build $(GHCFLAGS) --make

all: git-annex git-annex.1 docs

SysConfig.hs: configure.hs
	$(GHCMAKE) configure
	./configure

git-annex.1:
	./mdwn2man git-annex 1 doc/git-annex.mdwn > git-annex.1

git-annex: SysConfig.hs
	$(GHCMAKE) git-annex

install: all
	install -d $(DESTDIR)$(PREFIX)/bin
	install git-annex $(DESTDIR)$(PREFIX)/bin
	install -d $(DESTDIR)$(PREFIX)/share/man/man1
	install -m 0644 git-annex.1 $(DESTDIR)$(PREFIX)/share/man/man1
	install -d $(DESTDIR)$(PREFIX)/share/doc/git-annex
	if [ -d html ]; then \
		rsync -a --delete html/ $(DESTDIR)$(PREFIX)/share/doc/git-annex/html/; \
	fi

test:
	$(GHCMAKE) test
	./test

# If ikiwiki is available, build static html docs suitable for being
# shipped in the software package.
ifeq ($(shell which ikiwiki),)
IKIWIKI=@echo "** ikiwiki not found, skipping building docs" >&2; true
else
IKIWIKI=ikiwiki
endif

docs: git-annex.1
	$(IKIWIKI) doc html -v --wikiname git-annex --plugin=goodstuff \
		--no-usedirs --disable-plugin=openid --plugin=sidebar \
		--underlaydir=/dev/null --disable-plugin=shortcut \
		--disable-plugin=smiley \
		--exclude='news/.*'

clean:
	rm -rf build git-annex git-annex.1 test configure SysConfig.hs
	rm -rf doc/.ikiwiki html

.PHONY: git-annex test install
