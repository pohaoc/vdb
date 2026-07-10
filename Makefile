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
	rm -f bin/vdb bench/straggler

# Synthetic fork-join workload with a designated straggler, for exercising the
# sched pane (see the header of bench/straggler.c).
bench: bench/straggler

bench/straggler: bench/straggler.c
	gcc -O2 -Wall -Wextra -pthread -o $@ $<

.PHONY: default dist clean bench
