# Stargate Script Search Tool

This is a web app for searching transcripts from the Stargate TV
series. You can run it by first installing ElasticSearch and starting
it. The program assumes ElasticSearch is running on localhost at
post 9200.

Build the programs:

	$ stack build --copy-bins

Read the transcripts and output a suitable body to input the data into
ElasticSearch (the resulting file will be about 300k lines of text):

	$ stargate-parse > stargate.json

Input the data into ElasticSearch:

	$ curl -XGET 'localhost:9200/stargate/_search' --data-binary @stargate.json

Now you can run the web app:

	$ stargate-search-web
	Setting phasers to stun... (port 5000) (ctrl-c to quit)

And you can access it at localhost.

There is also an instance running at <http://stargate.kaashif.co.uk>.

## Current features

* Search by spoken phrase
* Search by character (who said it, who was it said to)
* Search by location

## Planned features

* Search by series
* Search by season/episode
* Multiple search queries
* Regular expressions

## Raw data

The raw data is just the transcripts. They are human-readable and
located in the `transcripts` directory. I wrote a parser that parses
them, so they are also machine-readable (see `Data.Stargate.Parse` for
the parser).

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
