SHELL=/bin/bash

all: test configure
	cabal build
	cp dist/build/jora2470_hw8/jora2470_hw8 .

test:
	if [[ "$$(which cabal)" == "" ]]; \
	then \
		echo cabal does not exist. Install it with \'sudo apt-get install cabal-install\'; \
		exit 1;\
	fi

configure:
#	cabal update
	cabal install cabal
	cabal install --only-dependencies
	cabal configure

clean:
	cabal clean
	- rm -f jora2470_hw8 *.hi *.o

superclean: clean
	- rm -f jora2470 $$(find . -name '.*sw*')
