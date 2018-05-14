# Stargate Script Search Tool

This is a web app for searching transcripts from the Stargate TV
series. You can run it by first installing ElasticSearch and starting
it. The program assumes ElasticSearch is running on localhost at
post 9200.

Build the programs:

	$ stack build --copy-bins

Read the transcripts and output a suitable body to input the data into
ElasticSearch (the resulting file will be about 300k lines of text):

	$ stargate-parse elasticsearch > stargate.json

Input the data into ElasticSearch:

	$ curl -XPOST 'localhost:9200/stargate/_bulk' --data-binary @stargate.json

Note, if you're running on a memory constrained environment (e.g. a
tiny VPS), then you may run into trouble inserting all 150k records at
the same time. Also performance isn't great doing that. Instead, split
the file into 1000 line chunks (make a directory to do this in, _lots_
of files will get created).

	$ split -l 1000 stargate.json stargate
	$ rm stargate.json # or move it somewhere else
	$ for f in *; do curl -XPOST 'localhost:9200/stargate/_bulk' --data-binary @${f}; done

That should reduce any possible breakage due to low memory. Also look
up how to reduce ElasticSearch's JVM heap size.

Create the prettified transcripts (so the web app doesn't have to
parse or do anything at runtime):

	$ ./make_pretty.sh

Now you can run the web app:

	$ stargate-search-web
	Setting phasers to stun... (port 5000) (ctrl-c to quit)

And you can access it at localhost.

There is also an instance running at <http://stargate.kaashif.co.uk>.

## Current features

* Search by spoken phrase
* Search by character (who said it, who was it said to)
* Search by location
* Search by series
* Search by season/episode
* Multiple search queries
* Regular expressions

Note: these features are inherited from ElasticSearch. The advanced
search page of the web app literally uses the ElasticSearch query
string syntax, so I get all of these features for free.

## Raw data

The raw data is just the transcripts. They are human-readable and
located in the `transcripts` directory. I wrote a parser that parses
them, so they are also machine-readable (see `Data.Stargate.Parse` for
the parser).

The web app itself doesn't read the raw transcripts, but preprocessed
and preformatted versions of them.

## Copyright

The scripts and anything I have written are under the MIT license. The
transcripts themselves are provided under the Fair Use doctrine of US
copyright law. The credit for typing out the transcripts goes to the
individual authors, the names of whom are included in each file.

The contents of this repository are for educational and entertainment
purposes only.

## AGPL License
Copyright (c) 2018 Kaashif Hymabaccus

This project is licensed under the AGPLv3, find a copy in the
`LICENSE` file.
