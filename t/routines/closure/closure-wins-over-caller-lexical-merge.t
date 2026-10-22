use Test;

# A compiled closure's free variable resolves to its own captured binding, not
# to a same-named lexical of whoever calls it (ADR-0055 slice 2). The closure
# keeps seeing a later write of its owner through the shared cell.
plan 7;

sub noop($v) { 1 }
my $b = "OUTER";
noop($b);
my $f = { $b };
sub collide() { my $b = "CALLER"; my $g = { $b }; $g.(); $f.() }
is collide(), 'OUTER', 'a closure called under a same-named caller lexical keeps its own';
is (1, 2).map({ $b }).join(','), 'OUTER,OUTER', 'also from a native map callback';
is ($f, ).map({ my $b = 'MAPPER'; $_() }).join, 'OUTER', 'and when the caller is itself a block';

my $shared = 0;
my @cbs;
for 1..3 { @cbs.push({ $shared }) }
$shared = 42;
is @cbs.map({ .() }).join(','), '42,42,42', 'a later write of the owner is seen through a loop-made closure';

my $w = 1;
my $cl;
EVAL q[$cl = { $w }];
$w = 77;
is $cl(), 77, 'a closure made inside EVAL tracks a later write of the caller lexical';

my $s = "pqab";
my $r := $s.substr-rw(1, 2);
$s = "WXYZ";
is $r, 'XY', 'a native Proxy re-fetches the source variable live';

my $x = 1;
my $k = do { my $y = 0; -> { ::('$x') } };
$x = 2;
is $k(), 2, 'a reflective closure reads the program scope live';
