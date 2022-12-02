.PHONY: test
test: Main sample1.txt sample2.txt sample3.txt sample4.txt sample5.txt sample6.txt sample7.txt sample8.txt input.txt
	./Main sample1.txt
	./Main sample2.txt
	./Main sample3.txt
	./Main sample4.txt
	./Main sample5.txt
	./Main sample6.txt
	./Main sample7.txt
	./Main sample8.txt
	./Main input.txt

Main: Main.hs
	ghc --make Main.hs -Wall -Wcompat -Widentities -Wincomplete-uni-patterns -Wincomplete-record-updates -Wredundant-constraints -Wmissing-export-lists -Wpartial-fields -Wmissing-deriving-strategies -Wunused-packages -fhide-source-paths -freverse-errors
