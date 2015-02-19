.PHONY: clean default

bin = main

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
	rm -f $(bin) $(intfs) $(objs)
