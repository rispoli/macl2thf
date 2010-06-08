CC = /usr/bin/mzc

all: macl2leoii

macl2leoii: macl2leoii.scm
	$(CC) --exe macl2leoii macl2leoii.scm

macl2leoii_dist: macl2leoii
	$(CC) --exe-dir macl2leoii_dist macl2leoii

clean:
	rm -r macl2leoii macl2leoii_dist
