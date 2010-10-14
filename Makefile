git-annex:
	mkdir -p build
	ghc -odir build -hidir build --make git-annex

clean:
	rm -rf build git-annex

.PHONY: git-annex
