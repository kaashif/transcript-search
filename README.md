# Transcript Search Tool

This is a web app for searching transcripts from the Stargate and Star
Trek TV series. There is an instance running at
<https://transcripts.kaashif.co.uk>.

You can run this app using Docker, but before you can build the image,
there are some things you need to do.

Build the parser program:

	$ stack build --copy-bins

Run the build script that regenerates the transcript data and the
prettified transcripts for the web app:

    $ ./build.sh

Now you can use `docker-compose` which will start up the database and
web app in separate containers.

    $ docker-compose up

Note that the first time you run this, the database container will
import the transcripts, which might take a long time. It will take so
long that the web app will probably time out connecting to the
database. Just wait for the importing to complete, then restart
everything. The second time around, the container will already have
the transcripts and it will work.

## Current features

* Search by spoken phrase
* Search by character (who said it, who was it said to)
* Search by location
* Search by series

## Planned features

* Search by season/episode
* Multiple search queries
* Regular expressions

I used to have these features from using ElasticSearch, but then I
switched to a plain old SQL server. This means I have to re-implement
these.

## Raw data

The raw data is just the transcripts. They are human-readable and
located in the `transcripts` directory. I wrote a parser that parses
them, so they are also machine-readable (see `Transcript.Parse` for
the parsers).

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
Copyright (c) 2018-2019 Kaashif Hymabaccus

This project is licensed under the AGPLv3, find a copy in the
`LICENSE` file.
