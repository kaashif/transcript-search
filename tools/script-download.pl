#!/usr/bin/perl
use strict;
use warnings;

use WWW::Mechanize;
use File::Slurp;

my $mech = WWW::Mechanize->new();
my $index = 'http://www.stargate-sg1-solutions.com/wiki/Transcripts';

sub trim_file {
  my $filename = shift;
  my $text = read_file($filename);
  open(my $fh, ">", $filename);
  my $start_printing = 0;
  foreach my $line (split /\n/, $text) {
	if ($line =~ /TEASER/) {
	  $start_printing = 1;
	}
	if ($line =~ /^Transcript$/) {
	  $start_printing = 1;
	  next;
	}
	if (!$start_printing) {
	  next;
	}
	if ($line =~ /Related Articles/) {
	  last;
	}
	$fh->print($line, "\n");
  }
}

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
	my $filename = "";
	if ($ep_page_link->text !~ /^Transcript$/) {
	  next;
	}
	if ($ep_page_link->text =~ /.*php.*/) {
	  next;
	}
	if ($ep_page_link->url !~ /\/wiki\/(SGA_|)(\d+.\d\d).*/) {
	  next;
	}
	$filename = $2;
	$filename = $series . "/". $filename;
	open(my $fh, ">", $filename . ".html");
	print "Downloading " . $filename . "\n";
	$mech->get($ep_page_link->url_abs);
	$fh->print($mech->content);
	system("lynx -dump $filename.html > $filename");
	trim_file $filename;
	unlink $filename . ".html";
  }
}
