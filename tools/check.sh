#!/bin/bash
for f in sg_script_search/{sg1,atl}/*; do
	printf "\n$f\n";
	runhaskell tools/scriptparse.hs $f | tail -n5
	printf "\n";
done
