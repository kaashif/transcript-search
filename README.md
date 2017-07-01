# Stargate Script Search Tool

This is a web app for searching transcripts from the Stargate TV
series. You can run it by cloning this repo and running:

	$ stack build
	$ stack exec stargate-search-web
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
* Command line interface

## Raw data

The raw data is just the transcripts. They are human-readable and
located in the `transcripts` directory. I wrote a parser that parses
them, so they are also machine-readable (see `Data.Stargate.Parse` for
the parser). I have also provided some CSV in the `data` directory,
which might be of interest if you want to do some analysis of your
own. The format is "season,episode,series,character,characters
present,location,text". There's a lot of redundancy storing the data
like this, but it is really easy to process. See `Data.Stargate.CSV`
for how the CSV munging is done.

## Copyright

The scripts and anything I have written are under the MIT license. The
transcripts themselves are provided under the Fair Use doctrine of US
copyright law. The credit for typing out the transcripts goes to the
individual authors, the names of whom are included in each file.

The contents of this repository are for educational and entertainment
purposes only.

## MIT License
Copyright (c) 2017 Kaashif Hymabaccus

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
