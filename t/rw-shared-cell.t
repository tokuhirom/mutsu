use v6;
use Test;

# Scalar `is rw`/`is raw` parameters bind a shared ContainerRef cell chained
# to the caller's variable (todo/tickets/rw-writeback-through-wrap-chain-
# needs-shared-cells.md): the caller's variable, the param, and any rw param
# the value is relayed to (wrap chain callsame, proto {*} redispatch) observe
# ONE container. Expected values verified against raku.

plan 14;

# The ticket repro: a wrap chain whose wrapper names its rw param DIFFERENTLY
# from the original's. The relay used to survive only by same-name env-merge
# coincidence; with cell binding every layer aliases one container.
my $wrapper-seen;
sub w5($x is rw) { $x = $x + 1; $x }
&w5.wrap(sub ($y is rw) { my $r = callsame(); $wrapper-seen = $y; "w5:$r" });
my $e = 40;
is w5($e), "w5:41", "wrap chain returns the wrapper's value";
is $wrapper-seen, 41, "wrapper's differently-named rw param observes the original's write";
is $e, 41, "the caller's variable observes the write through the chain";

# Plain rw writeback (compiled path, gate removed).
sub set9($x is rw) { $x = 9 }
my $a = 1;
set9($a);
is $a, 9, "plain rw writeback";

# rw chained through nested rw calls: one cell end to end.
sub outer($x is rw) { inner($x) }
sub inner($w is rw) { $w = 99 }
my $c = 0;
outer($c);
is $c, 99, "rw param relayed to a nested rw param writes the caller";

# Typed rw param: type check happens on the value, cell holds the coerced value.
sub typed(Int $x is rw) { $x = $x * 2 }
my Int $t = 21;
typed($t);
is $t, 42, "typed rw param writes back";

# is copy must NOT alias.
sub copies($x is copy) { $x = 3 }
my $k = 1;
copies($k);
is $k, 1, "is copy still copies";

# An element source keeps the exit copy-back (no cell replaces an array slot).
my @arr = 1, 2, 3;
sub elem($x is rw) { $x = 99 }
elem(@arr[1]);
is-deeply @arr, [1, 99, 3], "array element rw source still writes back";

# The rw alias survives the call: a closure over the param keeps mutating the
# caller's variable after the sub returned (raku: containers are aliased).
sub escape($x is rw) { -> { $x++ } }
my $v = 5;
my &bump = escape($v);
bump(); bump();
is $v, 7, "closure over an rw param keeps the caller alias after return";

# Post-call caller variable still behaves like a plain scalar.
my $p = 10;
sub touch($x is rw) { $x++ }
touch($p);
$p = $p + 5;
is $p, 16, "caller variable assignment after an rw call";
$p++;
is $p, 17, "caller variable increment after an rw call";

# Two rw params from the same source variable share one cell.
sub two($x is rw, $y is rw) { $x = $x + 1; $y = $y + 1 }
my $s = 0;
two($s, $s);
is $s, 2, "two rw params over one source share the cell";

# nextsame chain still relays the rw value (multi candidates).
multi sub chain(Int $x is rw) { $x = $x + 1; nextsame }
multi sub chain(Any $x is rw) { $x = $x + 10; $x }
my $m = 0;
chain($m);
is $m, 11, "nextsame rw chain accumulates through candidates";

# is raw with a literal argument stays readonly but callable.
sub rawlit($x is raw) { $x + 1 }
is rawlit(41), 42, "is raw accepts a literal";

done-testing;
