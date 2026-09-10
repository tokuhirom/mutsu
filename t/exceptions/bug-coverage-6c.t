use v6;
use Test;

# Regression pins for the fixes that made roast/6.c/MISC/bug-coverage.t pass.

plan 24;

# --- list-prefix precedence: a loose word-logical terminates a listop's args ---
# `is-deeply $x, $y, 'desc' orelse EXPR` is `(is-deeply ...) orelse EXPR`, so the
# right operand still runs. It used to be swallowed into the last argument.
{
    my @pulled;
    sub pull-it (\it) { @pulled.push: it.pull-one }
    my \iter = <a b c>.iterator;
    for ^3 {
        Nil andthen is-deeply 1, 1, 'never runs'
            orelse  pull-it iter;
    }
    is-deeply @pulled, ['a', 'b', 'c'], 'orelse after a listop call is not swallowed into its args';
    is iter.count-only, 0, 'the iterator really advanced';
}

# --- Iterator .count-only / .bool-only track consumption ---
{
    my \iter = <a b c>.iterator;
    is iter.count-only, 3, '.count-only before any pull';
    is iter.bool-only, True, '.bool-only before any pull';
    iter.pull-one;
    is iter.count-only, 2, '.count-only after one pull';
    iter.pull-one;
    iter.pull-one;
    is iter.count-only, 0, '.count-only after the last pull';
    is iter.bool-only, False, '.bool-only after the last pull';
}

# Buf/Blob iterate their elements, not themselves.
{
    my \bi = Buf.new(1, 2, 3).iterator;
    is bi.count-only, 3, 'Buf.iterator counts its bytes';
    is bi.pull-one, 1, 'Buf.iterator pulls a byte';
    is Blob.new(9, 8).iterator.count-only, 2, 'Blob.iterator counts its bytes';
}

# --- `with EXPR -> $p { }` keeps the enclosing topic ---
{
    $_ = 42;
    is-deeply (with 1 -> $a { .flip }), '24', 'with + pointy param does not rebind $_ (literal topic)';
    is $_, 42, '$_ is unchanged after the with';

    sub f { 9 }
    my $seen2;
    with f() -> $a { $seen2 = $_ }
    is $seen2, 42, 'with + pointy param does not rebind $_ (call topic)';

    # A plain `with` (no signature) still topicalizes.
    my $seen3;
    with 5 { $seen3 = $_ }
    is $seen3, 5, 'with without a signature still sets $_';
}

# An aliasing pointy param still writes through to an lvalue topic.
{
    my @a = 1, 2;
    with @a -> @p { @p.push(3) }
    is-deeply @a, [1, 2, 3], 'with @a -> @p aliases the source';

    my @b = 1, 2;
    with @b -> @p is copy { @p.push(3) }
    is-deeply @b, [1, 2], 'with @a -> @p is copy does not write back';
}

# --- a `for` body is its own placeholder scope ---
{
    my $blk = { my @r = gather for <a b> { $^v.uc andthen $v.take }; @r };
    is-deeply $blk(), ['a', 'b'], 'a $^v in a for body does not leak into the enclosing block signature';
    is $blk.arity, 0, 'the enclosing block takes no parameters';
}

# --- arithmetic sequences type their elements by `previous + step` ---
is-deeply (0.1, 2 ... 3).List, (0.1, 2.0), 'Rat, Int ... Int re-derives the Int seed as a Rat';
is-deeply (1.0, 2 ... 4).List, (1.0, 2.0, 3.0, 4.0), 'a Rat first seed makes the whole sequence Rat';
is-deeply (1, 3 ... 7).List, (1, 3, 5, 7), 'an all-Int arithmetic sequence stays Int';

# --- a non-dwimmy hyperop cannot broadcast a scalar against a longer list ---
throws-like ｢my $ = 5 »*» (2..4)｣, X::HyperOp::NonDWIM,
    'a scalar on the non-dwimmy side of a hyperop throws';
is-deeply (5 «*« (2, 3, 4)), (10, 15, 20), 'the same scalar broadcasts when its side dwims';
is-deeply ((5,) »*» (2, 3, 4)), (10,), 'a one-element list truncates the dwim side instead';
