use Test;

# ADR-0068 §2 lists five reasons the name-keyed cross-thread lane declines a
# container element store. Three of them had never been exercised by a probe:
# the name is not a plain lexical `@`/`%` (a twigil), the name is masked as
# re-declared, and the container was never in a spawning frame's env.
#
# Each block below reaches a container through exactly one of those reasons and
# writes 1000 elements from 20 concurrent threads. Measured with the §1.2
# oracle, the twigil'd and parameter-only routes land on the cell-keyed
# `ContainerStructGuard` and the re-declared route lands on the name-keyed
# `shared_array_elem_set` lane -- so all three are covered, by one funnel or the
# other, and none reaches an unguarded aliased store.
#
# These pin MUTSU's exclusion invariant, not a Raku guarantee: rakudo gives
# concurrent `@a[$i] = ...` no atomicity at all and loses updates on three of
# the four blocks below (measured 2026-09-08: 1 of 4 passing, on each of three
# runs). mutsu is deterministic here -- 0 failures in 384 runs at 24-way on 12
# cores under the gc-stress environment -- because the lane and the guard make
# it so, which is exactly what ADR-0068 set out to buy.

plan 4;

# Reason 1: the name is not a plain lexical `@`/`%`. `is_plain_lexical_name`
# requires an alphabetic second byte, so a dynamic's `*` twigil declines the
# atomic lane; the container is celled by the named sub instead.
{
    my @*log;
    sub put-it($i) { @*log[$i] = 1 }
    await (^20).map: -> $t { start { for ^50 -> $k { put-it($t * 50 + $k) } } };
    is @*log.grep(*.defined).elems, 1000,
        'every element store through a dynamic array lands';
}

# The hash twin of the same reason.
{
    my %*reg;
    sub set-it($k) { %*reg{$k} = 1 }
    await (^20).map: -> $t { start { for ^50 -> $k { set-it("k{$t * 50 + $k}") } } };
    is %*reg.elems, 1000, 'every key store through a dynamic hash lands';
}

# Reason 2: the name is masked as re-declared. A slurpy parameter of the same
# name records it in `thread_redeclared_vars`, which is keyed by NAME and not by
# scope, so the lane declines for the outer container's writes too.
{
    my @seen;
    sub helper(*@seen) { @seen.elems }
    sub put-seen($i) { @seen[$i] = 1 }
    helper(1, 2);
    await (^20).map: -> $t { start { for ^50 -> $k { put-seen($t * 50 + $k) } } };
    is @seen.grep(*.defined).elems, 1000,
        'every element store through a re-declared-masked name lands';
}

# Reason 4: the container was never in a spawning frame's env. It reaches the
# writer only as a parameter, and no thread body ever names it, so the escape
# analysis has nothing to see.
{
    my @a;
    sub worker(@dst, $i) { @dst[$i] = 1 }
    sub drive($i) { worker(@a, $i) }
    await (^20).map: -> $t { start { for ^50 -> $k { drive($t * 50 + $k) } } };
    is @a.grep(*.defined).elems, 1000,
        'every element store through a parameter-only container lands';
}
