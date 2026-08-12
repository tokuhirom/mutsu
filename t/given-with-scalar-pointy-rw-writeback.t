use Test;

# `given`/`with EXPR -> $v is rw {...}` binds `$v` as a genuine alias of
# EXPR, so a mutation through `$v` must write back to the source — a plain
# variable, a hash/array element, or `with`. Previously this silently lost
# the mutation: the pointy param's own synthetic `VarDecl` made it look like
# an ordinary block-local `my` to `exec_block_local_scope_op`, which Nil-reset
# its slot on block exit BEFORE the enclosing `given`/`with` op's writeback
# had a chance to read the final value (see
# todo/deep/pointy-scalar-param-final-value-untracked-by-writeback.md).
#
# A pointy scalar WITHOUT `is rw` should die on assignment ("Cannot assign to
# a readonly variable") per raku — mutsu does not enforce that (a separate,
# pre-existing gap noted in the ticket above, not fixed here) — so that case
# is intentionally not pinned in this file.

plan 8;

{
    my $x = 1;
    given $x -> $v is rw { $v += 10 }
    is $x, 11, 'given $x -> $v is rw: plain scalar writes back';
}

{
    my %h = a => 1, b => 2;
    given %h<a> -> $v is rw { $v += 10 }
    is %h<a>, 11, 'given %h<a> -> $v is rw: hash element accumulates back';
}

{
    my %h = a => 1;
    given %h<a> -> $v is rw { $v = 99 }
    is %h<a>, 99, 'given %h<a> -> $v is rw: whole reassignment writes back';
}

{
    my %h2 = a => 1, b => 2;
    with %h2<a> -> $v is rw { $v += 10 }
    is %h2<a>, 11, 'with %h2<a> -> $v is rw: hash element writes back';
}

{
    my @a = 1, 2, 3;
    given @a[0] -> $v is rw { $v += 10 }
    is @a[0], 11, 'given @a[0] -> $v is rw: array element writes back';
}

{
    # A pointy scalar param shadowing an outer variable of the SAME name must
    # not corrupt the outer binding once the block exits (regression check
    # for the fix's by-slot, not by-name, capture).
    my $x = 1;
    given 5 -> $x { $x += 100 }
    is $x, 1, 'given 5 -> $x (same name as outer): outer $x is untouched';
}

{
    # Nested given/with pointy scalars with the SAME param name must not
    # cross-contaminate each other's writeback.
    my $a = 1;
    my $b = 2;
    given $a -> $v is rw {
        given $b -> $v is rw {
            $v += 100;
        }
        $v += 10;
    }
    is $a, 11, 'nested given pointy scalar: outer writeback unaffected by inner';
    is $b, 102, 'nested given pointy scalar: inner writeback lands on its own source';
}

done-testing;
