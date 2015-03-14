.PHONY: clean default zip

bin = main

zipname = obstack-warlight2.zip

src = $(wildcard *.hs)
intfs = $(patsubst %.hs,%.hi,$(src))
objs = $(patsubst %.hs,%.o,$(src))

default: $(bin)

Main.hs: Graph.hs
Main.hs: Common.hs
Graph.hs: Common.hs

$(bin): $(src)
	ghc -o $@ $^

clean:
	rm -f $(bin) $(intfs) $(objs) $(zipname)

zip:
	zip $(zipname) Makefile $(src)
