use Test;

plan 9;

# Index-based upvalues: a closure captures its creator's lexical binding, not a
# same-name variable in whatever scope later calls it.
sub make-adder($x) { -> $y { $x + $y } }
my $add = make-adder(10);
my $x = 999;
is $add(5), 15, 'closure reads creator lexical, not caller same-name lexical';

# Read-only constant capture inside a map block.
my $base = 100;
is (1, 2, 3).map({ $_ + $base }).join(','), '101,102,103',
    'read-only constant capture in map block';

# Captured-and-mutated lexical: the closure observes the creator's later writes
# (shared container capture).
{
    my $v = 1;
    my $f = { $v };
    $v = 2;
    is $f(), 2, 'closure sees creator mutation after capture';
}

# Persistent per-closure state across calls.
sub make-counter() { my $n = 0; -> { $n++ } }
my $c = make-counter();
is $c(), 0, 'counter first call';
is $c(), 1, 'counter second call';
is $c(), 2, 'counter third call';

# Two closures from the same factory keep independent captured state.
my $c2 = make-counter();
is $c2(), 0, 'independent counter starts fresh';
is $c(), 3, 'original counter unaffected by sibling';

# Closures created in a loop over a shared OUTER lexical all see its final value
# (the outer variable is shared, not frozen per iteration).
{
    my @cbs;
    my $shared = 0;
    for 1..3 { @cbs.push({ $shared }) }
    $shared = 42;
    is @cbs.map({ .() }).join(','), '42,42,42',
        'loop closures share the outer lexical';
}
