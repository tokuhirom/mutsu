use Test;

plan 14;

# A closure's free variables are bound in the scope where the closure was
# *created*. When one closure calls another, the callee must read its own
# captured binding — never a same-named lexical that happens to live in the
# caller's frame. mutsu used to merge the captured env with "don't overwrite"
# into a child of the *caller's* env, so a caller lexical shadowed the callee's
# own capture: lexical scoping silently degraded into dynamic scoping.

{
    sub mk-leaf()   { my $me = 'LEAF'; return sub { $me } }
    sub mk-node($k) { my $me = 'NODE'; return sub { $me ~ '(' ~ $k.() ~ ')' } }
    is mk-node(mk-leaf()).(), 'NODE(LEAF)',
        'callee closure reads its own $me, not the calling closure\'s';
}

{
    # Same shape, one level deeper, with an array-typed capture: the merge had a
    # separate "never overwrite an Array with an Array" rule.
    sub mk-leaf($tag) { my @args; return sub { $tag } }
    sub mk-node($tag, @kids) {
        my @args = @kids;
        return sub { $tag ~ '[' ~ @args.map({ .() }).join(',') ~ ']' };
    }
    my $a = mk-leaf('a');
    my $b = mk-leaf('b');
    my $inner = mk-node('in', [$a, $b]);
    my $outer = mk-node('out', [$a, $inner]);
    is $outer.(), 'out[a,in[a,b]]',
        'sibling closures from one factory keep independent @args';
}

{
    # The self-recursion this used to cause blew the Rust stack instead of
    # terminating: the inner node re-read the outer node's @args (containing
    # itself) and recursed forever.
    sub mk-rec($depth) {
        my $d = $depth;
        return sub { $d <= 0 ?? 'end' !! $d ~ '>' ~ mk-rec($d - 1).() };
    }
    is mk-rec(4).(), '4>3>2>1>end', 'closure recursing through a fresh closure terminates';
}

# --- the existing capture semantics these fixes must not break ---

{
    my $g = 1;
    sub named-g() { $g }
    my $anon-g = sub { $g };
    $g = 99;
    is named-g(), 99, 'named sub sees a later mutation of a captured lexical';
    is $anon-g(), 99, 'anon closure sees a later mutation of a captured lexical';
}

{
    sub factory() { my $x = 1; my $f = sub { $x }; $x = 2; return $f }
    is factory().(), 2, 'closure sees mutation made after its creation';
}

{
    sub mk-counter() { my $c = 0; return sub { ++$c } }
    my $c1 = mk-counter();
    my $c2 = mk-counter();
    $c1(); $c1();
    is $c1(), 3, 'counter keeps its own per-instance mutable state';
    is $c2(), 1, 'a sibling counter has independent state';
}

{
    my @cl;
    for 1..3 -> $i { my $v = $i * 10; @cl.push(sub { $v }) }
    is @cl.map({ .() }).join(','), '10,20,30', 'per-iteration loop captures stay distinct';
}

# A dynamic variable is resolved through the caller chain on every call, so a
# closure must never freeze the value it saw on its first call.
{
    my $leaf = sub { $*TVAR };
    my @seen;
    for 1..3 -> $i {
        my $*TVAR = $i;
        @seen.push($leaf());
    }
    is @seen.join(','), '1,2,3', 'closure re-reads a dynamic var on every call';
}

{
    my @vars = <A B>;
    my $leafA = sub { %*VMAP<A> };
    sub row(@vals) {
        my %*VMAP = @vars Z=> @vals;
        $leafA();
    }
    is (row([1, 0]), row([0, 1]), row([0, 0])).join(','), '1,0,0',
        'closure re-reads a dynamic hash re-assigned per call';
}

# A self-recursive closure captures the variable it is being assigned to, so the
# capture runs before the declaration's store: only a shared cell can carry the
# value. Installing the (still unset) captured snapshot would make $rec read Any.
{
    my $rec = -> $n { $n <= 0 ?? 0 !! $n + $rec($n - 1) };
    is $rec(4), 10, 'self-recursive closure resolves itself';

    # ... and it must still resolve itself when invoked from an unrelated scope,
    # where the caller has no $rec of its own to fall back on.
    sub call-it(&f) { f(4) }
    is call-it($rec), 10, 'self-recursive closure works when called from another scope';
}

{
    # The declaration that captures must not be confused with a *later*
    # same-named redeclaration in an inner block: the closure keeps the outer $a.
    my $a = 3;
    my $bump = { $a++ };
    {
        my $a = -10;
        is ($bump(), $bump(), $a).join(','), '3,4,-10',
            'inner redeclaration does not write through the captured cell';
    }
}

# vim: expandtab shiftwidth=4
