#!/usr/bin/perl
use strict;
use warnings;

use WWW::Mechanize;
use File::Slurp;

my $mech = WWW::Mechanize->new();
my $index = 'http://www.stargate-sg1-solutions.com/wiki/Transcripts';

$mech->get($index);

my @links = $mech->links;
foreach my $link (@links) {
  my $series = "";
  if ($link->url !~ /\/wiki\/(Season|Stargate).*Transcripts.*/) {
	next;
  }
  if ($link->url =~ /\/Season.*/) {
	$series = "sg1";
  } elsif ($link->url =~ /.*Atlantis.*/) {
	$series = "atl"
  } else {
	next;
  }
  $mech->get($link->url_abs);
  my @index_links = $mech->links;
  foreach my $ep_page_link (@index_links) {
	if ($ep_page_link->text =~ /(\d+).(\d\d) \"(.+)\"/) {
	  print "$1.$2\@$3\n";
	}
  }
}
