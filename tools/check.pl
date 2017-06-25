#!/usr/bin/env perl
use strict;
use warnings;

my @transcript_fnames = glob("sg_script_search/{sg1,atl}/*");
my $i = 0;

while (1) {
  if ($i == scalar(@transcript_fnames)) {
	last;
  }
  printf $transcript_fnames[$i] . ": ";
  my $result = `timeout 3s runhaskell tools/scriptparse.hs $transcript_fnames[$i] | tail -n5`;
  if ($result =~ m/(CREDITS|THE END)/) {
	printf "SUCCEEDED\n";
	$i += 1;
  } else {
	printf "FAILED\nPlease correct and hit enter when done to retry.\n";
	printf $result;
	<STDIN>;
  }
}


