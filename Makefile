default:
	dune build

# Self-contained release binary in bin/vdb: statically linked and stripped, so it
# runs on any x86-64 Linux machine without OCaml, dune, opam, or even glibc installed.
dist:
	dune build --profile static bin/main.exe
	cp -f _build/default/bin/main.exe bin/vdb
	chmod u+w bin/vdb
	strip bin/vdb
	@ls -lh bin/vdb

clean:
	dune clean
	rm -f bin/vdb

.PHONY: default dist clean
