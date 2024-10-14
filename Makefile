# Used for `make test`
match=

# Used for `make test`
ifdef match
export match
endif

build:
	make hpack
	cabal build
.PHONY: build

hpack:
	hpack
.PHONY: hpack

run:
	make build
	cabal run exe:hasql-generator
.PHONY: run

test:
	make hpack
	cabal run hasql-generator-test -- $(if $(match), --match="$(match)")
.PHONY: test


ghciwatch:
	make hpack
	ghciwatch --command "cabal repl --repl-no-load" --error-file ghcid.txt --watch src --watch test --debounce 250ms
.PHONY: ghciwatch
