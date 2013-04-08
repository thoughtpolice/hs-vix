#!/usr/bin/env perl
use strict;
use warnings;

open(FH, 'error-codes.txt') or die "$!";
my ($fst, $snd, $thd);
my $ecode;
while(($fst = <FH>) && ($snd = <FH>) && ($thd = <FH>)) {
    chomp($fst); chomp($snd);
    my @ecode = split(/\s/, $fst);
    print "  , (#{const $ecode[2]}, \"$snd\")\n";
}

close(FH);
