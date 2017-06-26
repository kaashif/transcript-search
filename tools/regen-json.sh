#!/bin/bash

for f in sg_script_search/raw/{sg1,atl}/*; do
	echo $f
	tools/scriptparse.hs "$f" wantconvert > "${f/raw/json}.json"
done
