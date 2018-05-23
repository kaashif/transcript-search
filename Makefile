SRCS =	cli/Main.hs \
	web/Main.hs \
	web/ElasticSearch.hs \
	lib/Transcript/JSON.hs \
	lib/Transcript/IO.hs \
	lib/Transcript/Parse/Stargate.hs \
	lib/Transcript/Parse/Trek.hs \
	lib/Transcript/Markov.hs \
	lib/Transcript/Format.hs \
	lib/Transcript.hs

all: transcript-search-web transcript-parse

transcript-search-web transcript-parse: $(SRCS)
	vmctl start vm63
	doas ifconfig tap0 10.0.0.1
	until ssh 10.0.0.2 :; do sleep 1; done
	ssh 10.0.0.2 'cd ~/transcript-search; \
		git pull; \
		ksh -lc "stack build --copy-bins"'
	scp '10.0.0.2:~/.local/bin/transcript-*' ./

.PHONY: deploy
deploy: transcript-search-web
	scp transcript-search-web mail.kaashif.co.uk:~/
	ssh mail.kaashif.co.uk 'doas rm /usr/local/bin/transcript-search-web; \
		doas mv transcript-search-web /usr/local/bin/; \
		cd /home/kaashif/transcript-search; \
		git pull'

.PHONY: clean
clean:
	rm -f transcript-search-web transcript-parse
