.PHONY: test
test: Main sample.txt input.txt
	./Main sample.txt
	./Main input.txt

Main: Main.hs
	ghc --make Main.hs -Wall -Wcompat -Widentities -Wincomplete-uni-patterns -Wincomplete-record-updates -Wredundant-constraints -Wmissing-export-lists -Wpartial-fields -Wmissing-deriving-strategies -Wunused-packages -fhide-source-paths -freverse-errors
