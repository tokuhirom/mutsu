# A module whose file-scope lexicals are read and written by its own routines
# (t/concurrency/concurrent-unit-statics-shared.t).
unit module LateUnitStatics;
my $x = 42;
my %h = a => 1;
my int $n;
our sub get { "x=$x h={%h<a>} n=$n" }
our sub set($v) { $x = $v; %h<a> = $v; $n = $n + 1 }
