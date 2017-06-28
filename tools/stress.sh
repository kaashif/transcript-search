#!/bin/bash
for i in {1..25}; do
	echo $i;
	curl http://localhost:5000/search\?query\=apophis\&person\=\&present\=\&place\= > /dev/null;
done
