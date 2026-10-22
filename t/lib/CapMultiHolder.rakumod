use CapMultiStatics :mark;
unit class CapMultiHolder is Hash;
my %seen{Hash} = ();
my $total = 100;
method content { mark(self) }
method bump(Int $n) { add($n) }
method own-total { $total }
