#!/bin/bash
total=0
succeeded=0
for f in sg_script_search/{sg1,atl}/*; do
	total=$((total+1))
	printf "$f: ";
	result=$(timeout 3s runhaskell tools/scriptparse.hs $f | tail -n5)
	if [[ "$result" == *"END CREDITS"*  || "$result" == *"ROLL CREDITS"* ]]; then
		succeeded=$((succeeded+1))
		printf "SUCCEEDED\n"
	else
		printf "FAILED\n"
	fi
done
echo "Succeeded on $succeeded/$total"
