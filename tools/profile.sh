#!/bin/bash
set -e
rev=$(git show --format="%h" --no-patch)
echo Profiling $rev

rm -f *.prof *.ps *.png *.hp *.aux *.pdf *.mem
stack clean
stack build --profile
echo Running stargate-search
stack exec -- stargate-search +RTS -hc -p -sstargate-search.mem &
sleep 40
./tools/stress.sh
pkill stargate-search
hp2ps stargate-search.hp
ps2png stargate-search.ps stargate-searcha.png
convert -rotate -90 stargate-searcha.png stargate-search.png
for f in stargate-search.*; do
	mv "$f" "profiles/${f/stargate-search/$rev}"
done
git checkout stargate-search.cabal
