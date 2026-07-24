unit module OurVarExport;

our $val is export;
our @list is export;
our &code is export;

sub mk-counter { my $i = 0; sub { $i++ } }

$val = 7;
@list = 1, 2, 3;
&code = mk-counter;
