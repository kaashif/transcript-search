#!/bin/bash -x

if [[ -d random ]]; then
	echo "random directory already exists"
	exit 1
fi

mkdir random
for s in transcripts/{tos,tng,ds9,voy,ent}; do
	series=$(basename $s)
	echo "${series}"
	transcripts/markov.py transcripts/${series} random/${series}
	for e in random/${series}/*.txt; do
		transcript-parse single_trek <$e >${e%.txt}.pretty
	done
done
