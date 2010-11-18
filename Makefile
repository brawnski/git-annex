all: git-annex docs

ghcmake=ghc -Wall -odir build -hidir build -O2 --make 

SysConfig.hs:
	$(ghcmake) configure
	./configure

git-annex: SysConfig.hs
	$(ghcmake) git-annex

install:
	install -d $(DESTDIR)/usr/bin
	install git-annex $(DESTDIR)/usr/bin

# If ikiwiki is available, build static html docs suitable for being
# shipped in the software package.
ifeq ($(shell which ikiwiki),)
IKIWIKI=@echo "** ikiwiki not found, skipping building docs" >&2; true
else
IKIWIKI=ikiwiki
endif

test:
	$(ghcmake) test
	./test

docs:
	./mdwn2man git-annex 1 doc/git-annex.mdwn > git-annex.1
	$(IKIWIKI) doc html -v --wikiname git-annex --plugin=goodstuff \
		--no-usedirs --disable-plugin=openid --plugin=sidebar \
		--underlaydir=/dev/null --disable-plugin=shortcut \
		--disable-plugin=smiley

clean:
	rm -rf build git-annex git-annex.1 test configure SysConfig.hs
	rm -rf doc/.ikiwiki html

.PHONY: git-annex test install
