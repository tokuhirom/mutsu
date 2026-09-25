use Test;
use lib 't/lib';

# #9336: a proto body is run by `call_proto_function`, which restores the
# caller's env afterwards and carries over what the body rebound. That
# carry-over walked only the saved env's own overlay tier, so a caller
# variable visible only through a PARENT tier (any variable of an enclosing
# scope seen from inside a bare block / loop body) lost the rebind: a multi's
# `@array does R` never reached the caller's `@a`.

plan 6;

{
    proto sub f(|) {*}
    multi sub f(@array is raw) { role Q { }; (@array does Q); 1 }
    my @a = <a b c>;
    { f(@a); }
    is @a.^name, 'Array+{Q}', 'does on an is-raw array param reaches a caller inside a nested block';
}

{
    proto sub bump(|) {*}
    multi sub bump(@array is raw) {
        role Counter { has $!i = 0; method bump { $!i++ } }
        @array ~~ Counter ?? @array.bump !! (@array does Counter).bump
    }
    my @a = 1, 2;
    my @seen;
    { @seen.push: bump(@a); @seen.push: bump(@a); }
    is-deeply @seen, [0, 1], 'the mixed-in state persists across calls from a nested block';
}

{
    use ProtoMultiEachMixin;
    my @a = <a b c>;
    my @got;
    my $n = 0;
    while each(@a) -> ($k, $v) { @got.push: "$k $v"; last if ++$n > 3 }
    my $z = 1;
    is-deeply @got, ['0 a', '1 b', '2 c'], 'P5each: imported multi in a while condition with a statement after the loop';
}

{
    use ProtoMultiEachMixin;
    my @a = <x y>;
    my @got;
    { my $x; while ($x = each(@a)) { @got.push: $x.join(' ') } }
    is-deeply @got, ['0 x', '1 y'], 'P5each: loop nested inside a bare block';
}

# The carry-over is limited to names the caller can already see: a proto
# body's own `my` must not leak into a same-named caller lexical.
{
    my $x = 1;
    proto sub g(|) { my $x = 5; {*} }
    multi sub g($a) { $a }
    { g(3); }
    is $x, 1, "a proto body's own lexical does not clobber the caller's";
}

{
    my $n = 10;
    proto sub h($n) { {*} }
    multi sub h($n) { $n + 1 }
    my $r;
    { $r = h(1); }
    is "$r $n", '2 10', "a proto parameter does not clobber the caller's same-named lexical";
}
