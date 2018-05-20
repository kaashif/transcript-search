#!/usr/bin/env bash

if [[ -d pretty ]]; then
	echo "pretty directory already exists"
	exit 1
fi

mkdir pretty
for s in transcripts/{sg1,atl}; do
	series=$(basename $s)
	mkdir pretty/${series}
	for e in transcripts/${series}/*; do
		episode=$(basename $e)
		echo "$series : $episode"
		transcript-parse single_gate <transcripts/${series}/${episode} >pretty/${series}/${episode}
	done
done
for s in transcripts/{tos,tng,ds9,voy,ent}; do
	series=$(basename $s)
	mkdir pretty/${series}
	for e in transcripts/${series}/*; do
		episode=$(basename $e)
		echo "$series : $episode"
		transcript-parse single_trek <transcripts/${series}/${episode} >pretty/${series}/${episode}
	done
done

