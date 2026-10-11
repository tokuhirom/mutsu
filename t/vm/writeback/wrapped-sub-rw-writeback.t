use Test;
plan 6;

# #12521: an `is rw` argument of a wrapped routine reaches the wrapper's
# `|c` capture as the caller's container, also for a sub-local variable.
sub plain(uint32 $x is rw) { $x = 5 }
&plain.wrap(-> |c { c[0] = 7; Nil });
sub t1 { my uint32 $l = 1; plain($l); $l }
is t1(), 7, 'sub-local is rw arg written through a wrapper';
my uint32 $g = 1;
plain($g);
is $g, 7, 'file-scope is rw arg written through a wrapper';

sub other($x is rw) { $x = 5 }
&other.wrap(-> |c { callsame });
sub t2 { my $l = 1; other($l); $l }
is t2(), 5, 'callsame from a |c wrapper reaches the original';

# Index-assigning a Capture element keeps the Capture and the container.
my $r = 1;
my $w = -> |c { c[0] = 7; c.WHAT };
is $w($r).gist, '(Capture)', 'c[0] = ... keeps a Capture';
is $r, 7, 'c[0] = ... writes through the argument container';
my $s = 1;
my $cap = \($s);
$cap[0] = 4;
is $s, 4, '\($s) capture element assignment writes through';
