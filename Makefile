CC = /usr/bin/mzc

all: macl2thf macl2thf_dist

macl2thf: macl2thf.scm
	$(CC) --exe macl2thf macl2thf.scm

macl2thf_dist: macl2thf
	$(CC) --exe-dir macl2thf_dist macl2thf

clean:
	rm -r macl2thf macl2thf_dist
