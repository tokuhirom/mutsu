use Test;

plan 8;

# Read-only constant capture inside a map block (read via an upvalue / live env).
my $base = 100;
is (1, 2, 3).map({ $_ + $base }).join(','), '101,102,103',
    'read-only capture in map block';

# Captured-and-mutated lexical: the closure observes the creator's later writes
# through the shared container cell (captured as an upvalue).
{
    my $v = 1;
    my $f = { $v };
    $v = 2;
    is $f(), 2, 'closure sees creator mutation after capture';
}

# Persistent per-closure state across calls (boxed cell upvalue).
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
