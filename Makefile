git-annex:
	mkdir -p build
	ghc -odir build -hidir build --make git-annex

install:
	install -d $(DESTDIR)/usr/bin
	install git-annex $(DESTDIR)/usr/bin

clean:
	rm -rf build git-annex

.PHONY: git-annex
