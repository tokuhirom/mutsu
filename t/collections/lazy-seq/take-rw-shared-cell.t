use Test;

plan 13;

# `take-rw <lvalue>` must capture the *source container* (a shared cell), so the
# gathered value keeps container identity (`=:=`) with the original element and a
# write through one is observed by the other. Mirrors roast/S04-statements/gather.t
# test 38 (take-rw reference equality through nested indexing).

# Single-level element, stored into an array element (item context preserves the
# gathered Seq's container elements).
{
    my @spot = 10, 20, 30;
    my @n;
    @n[0] = eager gather { take-rw @spot[1] };
    is @n[0][0], 20, 'take-rw gathered the element value';
    ok @n[0][0] =:= @spot[1], 'take-rw keeps container identity (=:=)';
    @n[0][0] = 999;
    is @spot[1], 999, 'writing through the taken cell updates the source element';
}

# Nested element through a for-gather (the gather.t shape).
{
    my @spot = [10, 20, 30], [40, 50, 60];
    my @neighbors;
    @neighbors[0] = eager gather for 0, 1, 2 { take-rw @spot[0][$_] };
    is @neighbors[0][1], 20, 'nested take-rw gathered the right value';
    ok @neighbors[0][1] =:= @spot[0][1], 'nested take-rw keeps container identity';
}

# A plain `take` must NOT alias — it snapshots the value (no container identity).
{
    my @spot = 10, 20, 30;
    my @n;
    @n[0] = eager gather { take @spot[1] };
    nok @n[0][0] =:= @spot[1], 'plain take is a snapshot, not a live alias';
}

# Out-of-range element under `// next`: the cell wraps an undefined value, so the
# `//` fallback must still fire (value_is_defined looks through the cell).
{
    my @spot = 10, 20, 30;
    my @got = eager gather for 0, 5 { take-rw @spot[$_] // next };
    is @got.elems, 1, '// next skips the undefined (out-of-range) element';
}

# A plain scalar has no subscript terminal to promote.  `take-rw` itself must
# box that named scalar, and the gathered topic must write through the cell.
{
    my $x = 1;
    for (gather { take-rw $x }) { $_ = 42 }
    is $x, 42, 'take-rw of a plain scalar writes through its promoted cell';
}

# ADR-0045 makes a `for` topic the array element container.  `take-rw $_`
# must retain that existing cell rather than decontainerizing it before take.
{
    my @a = 1, 2, 3;
    sub aliases(@list) { gather for @list { take-rw $_ } }
    for aliases(@a) { $_ = $_ * 10 }
    is-deeply @a, [10, 20, 30], 'take-rw of the for topic retains each element cell';

    my @b = 1, 2, 3;
    my @g = gather for @b { take-rw $_ };
    @g[0] = 99;
    is-deeply @b, [99, 2, 3], 'a stored take-rw topic alias writes through after gather';
}

# https://github.com/Raku/old-issue-tracker/issues/4668 / mutsu #8521: an
# inline scalar declaration (`my $ = ...`) used as the `take-rw` operand has
# no pre-existing container an element subscript would promote, so the
# compiler must mint a fresh one and retain it -- same as a bare `Expr::Var`
# operand. Pulled through the LAZY (coroutine) `AT-POS` method-call path,
# NOT `eager`/a subscript-read, which is what previously masked this: an old
# buggy element-store fallback silently converted the whole `$l` from a
# `Seq`/`LazyList` into a plain `Array` as a side effect of the assignment,
# which happened to still answer 42 on a later read while losing the real
# container identity and the value's type.
{
    my $l = gather { take-rw my $ = 1 };
    $l.AT-POS(0) = 42;
    is $l.AT-POS(0), 42, 'AT-POS on a gather Seq with take-rw of an inline decl works';
    isa-ok $l, Seq, 'the gathered Seq keeps its type instead of decaying to Array';
}

# `.AT-POS($i)` is the same `postcircumfix:<[ ]>` protocol method `$l[$i]`
# compiles to (Language/subscripts.rakudoc), so it must pull only as many
# elements as the index needs, not force the whole (here: infinite) LazyList
# -- a regression here would hang forever rather than fail cleanly.
{
    sub gen() { take 1; take 2 }
    my $s = gather { loop { gen() } };
    is $s.AT-POS(2), 1, 'AT-POS on an infinite gather stays lazy, like [$i]';
}
