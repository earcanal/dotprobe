#!/usr/bin/perl
# vi: ft=perl

use strict;
use warnings;
use Text::CSV;
use Data::Dumper;
use Template;

my $debug = 0;

my $datadir = '/media/paul/2E95-1293/study/participants';
my $csv     = Text::CSV->new()
                 or die "Cannot use CSV: ".Text::CSV->error_diag ();

my(@participants);
my $schedules_f = "${datadir}/rumination study - schedules.csv";
open my $schedules, "<:encoding(utf8)", $schedules_f or die "$schedules_f $!";
my $header = $csv->getline($schedules);
for (my $i = 1; $i <= 12; $i++ ) {
  my %p       = ();
  my $row     = $csv->getline($schedules);
  $p{id}      = $row->[0];
  $row->[1]   =~ m#^(\d+)/#;
  $p{a}       = $1;
  my @data    = ();
  my($i,$s)   = (0,1);
  my(@cells)  = @$row[3..$#{$row}];
  for my $cell ( @cells ) {
    if ($cell eq '') {
      push(@data,'NA')
    } elsif ($cell eq 'bad') {
      push(@data,'bad');
      $s++;
    } elsif ($cell eq 'postpone') {
      push(@data,'X');
    } else {
      push(@data,$s);
      $s++;
    }
  }
  $p{sessions} = $s - 1;
  $p{data}     = [ @data ];
  push(@participants,{ %p });
}

my $tt = Template->new(
  INCLUDE_PATH => '/home/paul/Documents/psychology/msc/M210/apprenticeship/opensesame/dotprobe',
  POST_CHOMP   => 1
);
$tt->process('completed.tt', { h => $header, participants => \@participants } ) || die $tt->error;
