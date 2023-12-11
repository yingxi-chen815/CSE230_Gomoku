STACK       = stack

.PHONY: all test build clean distclean server client

all: test

server:
	$(STACK) build
	$(STACK) exec my-haskell-project-exe server

client:
	$(STACK) build
	$(STACK) exec my-haskell-project-exe client

test: clean
	$(STACK) test --test-arguments="--num-threads 1"

clean:
	
distclean: clean
	$(STACK) clean
