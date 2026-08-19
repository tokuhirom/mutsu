use Test;

# `eqv` on a List whose elements are raw Proxy containers (not wrapped in a
# Scalar, e.g. `.map`'s return values) must FETCH each element before
# comparing -- matching raku, which reads through a container on any
# value-context access. `eval_binary_with_junctions` already auto-FETCHes a
# *top-level* Proxy operand; this pins the deeper case, nested inside an
# Array/List/Hash/Pair.

plan 6;

my $v = 5;
my $p = Proxy.new(
    FETCH => method () { $v },
    STORE => method ($n) { $v = $n },
);

is $p eqv 5, True, 'top-level Proxy operand still FETCHes (pre-existing)';

my $l = (1, 2).map({
    my $inner = $_;
    Proxy.new(
        FETCH => method () { $inner },
        STORE => method ($n) { $inner = $n },
    );
}).List;

is-deeply $l, $(1, 2), 'nested Proxy list elements FETCH for is-deeply (eqv)';
ok $l eqv $(1, 2), 'nested Proxy list elements FETCH for a direct eqv';
nok $l eqv $(1, 3), 'a genuinely different nested Proxy list is not eqv';

my %h = a => $p;
ok %h eqv { a => 5 }, 'a Proxy nested inside a Hash value FETCHes for eqv';

my $pair = (a => $p);
ok $pair eqv (a => 5), 'a Proxy nested inside a Pair value FETCHes for eqv';
