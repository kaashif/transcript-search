ssh aws "sudo systemctl stop stargate-search; rm -rf stargate-search/*"
rsync -a transcripts/ aws:~/stargate-search/transcripts/
rsync -a templates/ aws:~/stargate-search/templates/
stack install
scp ~/.local/bin/stargate-search-web aws:~/stargate-search/
ssh aws "sudo systemctl start stargate-search"

