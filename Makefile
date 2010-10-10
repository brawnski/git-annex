git-annex:
	ghc --make git-annex

clean:
	rm -f git-annex *.o *.hi *.ho *.a

.PHONY: git-annex
