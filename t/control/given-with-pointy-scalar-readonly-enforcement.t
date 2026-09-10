use Test;

# `given`/`with EXPR -> $v {...}` (no `is rw`) must reject an assignment to
# `$v` with "Cannot assign to a readonly variable" per raku, regardless of
# whether the topic source itself is mutable — see
# todo/tickets/given-with-pointy-scalar-missing-readonly-enforcement.md
# (now resolved). `@`/`%`-sigil pointy params stay writable unconditionally
# (raku binds them rw with no trait needed); `is rw` lifts the scalar case.
#
# The readonly check must fire even when the assignment is the block's tail
# statement — a separate, more general compiler gap (`given`/`when` tail
# assignments skipped the readonly check entirely) was found and fixed
# alongside this ticket; see the plain (non-tail) vs tail-position pins below.

plan 9;

{
    my $x = 1;
    dies-ok { given $x -> $v { $v = 99 } },
        'given $x -> $v (no is rw, tail position): assignment dies';
    is $x, 1, 'given $x -> $v: source is untouched after the die';
}

{
    my $x = 1;
    dies-ok { given $x -> $v { $v = 99; 1 } },
        'given $x -> $v (no is rw, non-tail position): assignment dies';
}

{
    dies-ok { given 42 -> $v { $v = 99 } },
        'given 42 -> $v (literal topic, no is rw): assignment dies';
}

{
    my $x = 1;
    dies-ok { with $x -> $v { $v = 99 } },
        'with $x -> $v (no is rw): assignment dies';
}

{
    my $x = 1;
    given $x -> $v is rw { $v = 99 }
    is $x, 99, 'given $x -> $v is rw: assignment still writes back';
}

{
    # Native pointy params don't support `is rw` writeback at all yet (a
    # separate, pre-existing gap — see
    # todo/tickets/native-pointy-param-is-rw-writeback-missing.md); this pin
    # is scoped to the readonly-without-rw enforcement only.
    my int $x = 1;
    dies-ok { given $x -> int $v { $v = 99 } },
        'given $x -> int $v (native, no is rw): assignment dies';
}

{
    my @a = 1, 2, 3;
    given @a -> @p { @p[0] = 99 }
    is @a[0], 99, 'given @a -> @p (no trait): array pointy stays writable';
}

{
    my %h = a => 1;
    given %h -> %p { %p<a> = 99 }
    is %h<a>, 99, 'given %h -> %p (no trait): hash pointy stays writable';
}

done-testing;
