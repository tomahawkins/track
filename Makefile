.PHONY: all
all:
	runhaskell -W Track.hs

.PHONY: clean
clean:
	-rm *.svg

