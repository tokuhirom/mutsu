use Test;

# Two rules this file pins, both surfaced by the vendored upstream
# `Test.rakumod` (its `is-deeply` narrows `Seq` arguments with `.cache` and
# compares with `eqv`; its `cmp-ok` reaches an operator only through
# `&CALLER::LEXICAL::("infix:<...>")`, i.e. the ROUTINE form):
#
#   1. `.cache` on a not-yet-reified `Seq` returns a value that IS a `List`
#      everywhere it is asked -- `.^name`/`.WHAT`, type matching, `eqv`, and
#      `.raku` -- not only in its type name (ADR-0038 S2).
#   2. `&infix:<eqv>($a, $b)` behaves exactly like `$a eqv $b`, including the
#      Seq reify/consume protocol that raises `X::Seq::Consumed`.

plan 41;

sub make-deferred() {
    Seq.new(class :: does Iterator {
        has @!stuff = <a b c>;
        method pull-one { @!stuff and return @!stuff.shift; IterationEnd }
    }.new)
}

# --- 1. `.cache` on a deferred Seq is a List in every facet ----------------

{
    my $cached = make-deferred().cache;
    is $cached.^name, 'List', '.cache on a deferred Seq is named List';
    ok $cached ~~ List, '.cache result smartmatches List';
    nok $cached ~~ Seq, '.cache result does not smartmatch Seq';
    ok $cached eqv ('a', 'b', 'c'), '.cache result is eqv to a List';
    nok $cached eqv <a b c>.Seq, '.cache result is NOT eqv to a Seq';
    nok $cached eqv ['a', 'b', 'c'], '.cache result is NOT eqv to an Array';
    is $cached.elems, 3, '.cache result has the right elements';
}

{
    my $cached = make-deferred().cache;
    is $cached.raku, '$("a", "b", "c")',
        'a deferred cache List is itemized by scalar assignment';
    is $cached.^name, 'List', 'an itemized deferred cache handle is still a List';
}

{
    my $cached = Seq.new(class :: does Iterator {
        method pull-one { die 'scalar assignment forced the cached Seq' }
    }.new).cache;
    pass 'scalar assignment leaves a deferred cache source untouched';
}

{
    # Rendered without a Scalar container so the assertion is about the
    # value's TYPE, not about itemization: a List renders `(...)`, a Seq
    # renders `(...).Seq`.
    is make-deferred().cache.raku, '("a", "b", "c")',
        '.cache on a deferred Seq renders as a List';
    is <a b c>.Seq.cache.raku, '("a", "b", "c")',
        '.cache on an eager Seq renders as a List';
    is <a b c>.Seq.raku, '("a", "b", "c").Seq', 'an uncached Seq renders as a Seq';
}

# The same, for an eagerly-built Seq (the arm that already worked -- pinned so
# the two representations cannot drift apart again).
{
    my $cached = <a b c>.Seq.cache;
    is $cached.^name, 'List', '.cache on an eager Seq is named List';
    ok $cached eqv ('a', 'b', 'c'), 'eager .cache result is eqv to a List';
    nok $cached eqv <a b c>.Seq, 'eager .cache result is NOT eqv to a Seq';
}

# The original Seq keeps its own type: the List view is a property of the
# HANDLE, not of the shared body.
{
    my $s = make-deferred();
    my $c = $s.cache;
    is $s.^name, 'Seq', 'the original handle is still a Seq';
    is $c.^name, 'List', 'the .cache handle is a List';
    ok $s eqv <a b c>.Seq, 'the original handle is still eqv to a Seq';
}

# A plain (non-deferred) Seq compares type-strictly, as always.
{
    ok <a b c>.Seq eqv <a b c>.Seq, 'Seq eqv Seq';
    nok <a b c>.Seq eqv ('a', 'b', 'c'), 'Seq is not eqv to a List';
    nok ('a', 'b', 'c') eqv ['a', 'b', 'c'], 'List is not eqv to an Array';
    ok ('a', 'b', 'c') eqv ('a', 'b', 'c'), 'List eqv List';
}

# --- 2. the routine form of an operator is the operator -------------------

{
    my $eqv = &infix:<eqv>;
    ok $eqv((1, 2, 3), (1, 2, 3)), '&infix:<eqv> agrees on two Lists';
    nok $eqv((1, 2, 3), [1, 2, 3]), '&infix:<eqv> is type-strict';
    ok $eqv(make-deferred().cache, ('a', 'b', 'c')),
        '&infix:<eqv> sees the .cache List view too';
    nok $eqv(make-deferred().cache, <a b c>.Seq),
        '&infix:<eqv> rejects List view vs Seq';
}

# A consumed Seq throws `X::Seq::Consumed` from BOTH the operator and the
# routine form. This is what `cmp-ok $s1, 'eqv', $s2` needs.
{
    (my $s1 = (1, 2, 3).Seq.slice(0, 1, 2)).sink;
    (my $s2 = (3, 4, 5).Seq.slice(0, 1, 2)).sink;
    my $r = try { $s1 eqv $s2 };
    nok $r.defined, 'operator eqv on a consumed Seq does not answer';
    isa-ok $!, X::Seq::Consumed, 'operator eqv throws X::Seq::Consumed';
}

{
    (my $s1 = (1, 2, 3).Seq.slice(0, 1, 2)).sink;
    (my $s2 = (3, 4, 5).Seq.slice(0, 1, 2)).sink;
    my $eqv = &infix:<eqv>;
    my $r = try { $eqv($s1, $s2) };
    nok $r.defined, 'routine eqv on a consumed Seq does not answer';
    isa-ok $!, X::Seq::Consumed, 'routine eqv throws X::Seq::Consumed';
}

{
    (my $s1 = (1, 2, 3).Seq.slice(0, 1, 2)).sink;
    (my $s2 = (3, 4, 5).Seq.slice(0, 1, 2)).sink;
    throws-like { cmp-ok $s1, 'eqv', $s2 }, X::Seq::Consumed,
        'cmp-ok with the "eqv" operator name throws on a consumed Seq';
}

# ... and the reduction / metaop forms, which reach the operator by a third
# route again (`eval_reduction_operator_values`).
{
    (my $s1 = (1, 2, 3).Seq.slice(0, 1, 2)).sink;
    (my $s2 = (3, 4, 5).Seq.slice(0, 1, 2)).sink;
    my $r = try { [eqv] $s1, $s2 };
    nok $r.defined, '[eqv] on a consumed Seq does not answer';
    isa-ok $!, X::Seq::Consumed, '[eqv] throws X::Seq::Consumed';
}

{
    ok ([eqv] (1, 2, 3), (1, 2, 3)), '[eqv] agrees on two Lists';
    is-deeply ((1, 2) Zeqv (1, '2')).List, (True, False),
        'Zeqv compares element-wise and stays type-strict';
}

# --- 3. is-deeply's own narrowing, end to end ------------------------------

is-deeply make-deferred(), <a b c>.Seq, 'is-deeply: deferred Seq vs eager Seq';
is-deeply <a b c>.Seq, <a b c>.Seq, 'is-deeply: eager Seq vs eager Seq';
is-deeply <a b c>.Seq, ('a', 'b', 'c'), 'is-deeply: Seq vs List';
is-deeply make-deferred(), ('a', 'b', 'c'), 'is-deeply: deferred Seq vs List';

{
    my @res = <a b c>;
    is-deeply @res.Seq, ('a', 'b', 'c').cache,
        'is-deeply: Seq vs a cached List (the S16-io/words.t shape)';
}

# vim: expandtab shiftwidth=4
