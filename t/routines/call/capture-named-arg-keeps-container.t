use Test;
plan 9;

# A `|c` capture keeps the caller's container of a NAMED argument as it does
# for a positional one, so forwarding it to an `is raw` parameter writes the
# caller's variable (#16121; Deps' `deps-root :deps(my $root)`).
sub w(:$p is raw) { $p = 5 }
sub via(|c) { w |c }

my $a;
via :p($a);
is $a, 5, 'named argument forwarded through a sub |c';

my $b;
(-> |c { w |c })(:p($b));
is $b, 5, 'named argument forwarded through an immediately called pointy block';

sub root(&block, :$deps is raw) { $deps = (state $n = 0)++ + 100; block }
my &wrap = -> |c { root |c };

my $c;
wrap :deps($c), { is $c, 100, 'a block sees the write while the call runs' };
is $c, 100, 'the variable holds it afterwards';

# A variable declared inside the argument, read by the block beside it.
wrap :deps(my $d), { is $d, 101, 'declared in the argument: the block sees it' };
is $d, 101, 'declared in the argument: the variable holds it afterwards';

# Nested, each root keeps its own variable (the Deps shape).
my @seen;
wrap :deps(my $r1), {
    wrap :deps(my $r2), {
        @seen.push: $r1, $r2;
    }
}
is-deeply @seen, [102, 103], 'nested forwards keep one variable per root';

# An undefined variable is still a variable, not a type name.
sub set-it(:$x is raw) { $x = 'set' }
sub fwd(|c) { set-it |c }
my $u;
fwd :x($u);
is $u, 'set', 'an undefined variable is written through |c';

# The positional form keeps working.
sub wp($p is raw) { $p = 6 }
sub viap(|c) { wp |c }
my $e;
viap $e;
is $e, 6, 'positional form still works';
