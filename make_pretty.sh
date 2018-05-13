#!/usr/bin/env bash

if [[ -d pretty ]]; then
	echo "pretty directory already exists"
	exit 1
fi

mkdir pretty
for s in transcripts/*; do
	series=$(basename $s)
	mkdir pretty/${series}
	for e in transcripts/${series}/*; do
		episode=$(basename $e)
		echo "$series : $episode"
		stargate-parse single <transcripts/${series}/${episode} >pretty/${series}/${episode}
	done
done

