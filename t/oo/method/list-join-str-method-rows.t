use Test;

# ADR-11276 slice 3: `List.join` (with and without a separator) and `Str`
# on `Str` and the numeric types are rows in the built-in method table. A
# row declines what needs the interpreter: an element with a user `Str`, a
# Junction, an undefined element (Rakudo warns for it), a zero-denominator
# rational's `.Str`.

plan 10;

subtest 'List.join', {
    plan 6;
    my @a = 1, "b", 1.5, 1e0;
    is-deeply @a.join, "1b1.51", 'no separator';
    is-deeply @a.join("-"), "1-b-1.5-1", 'a separator';
    is-deeply (1, (2, 3), [4, 5]).join(","), "1,2 3,4 5", 'nested lists stringify';
    is-deeply (1, 2, 3).join(0), "10203", 'a numeric separator';
    is-deeply ().join(","), "", 'an empty list';
    is-deeply (^3).map({ @a.join(":") }).List, ("1:b:1.5:1" xx 3).List, 'one site, repeated';
}

subtest 'holes and defaults', {
    plan 2;
    my @d is default(5);
    @d[2] = 1;
    is-deeply @d.join, "551", 'a hole reads as the default';
    is-deeply @d.join(","), "5,5,1", '... with a separator too';
}

subtest 'elements that need the interpreter', {
    plan 4;
    my class C { method Str { "C!" } }
    is-deeply (1, C.new).join("+"), "1+C!", 'a user Str';
    is-deeply [C.new].join, "C!", '... without a separator';
    is-deeply (IntStr.new(1, "one"), 2).join(","), "one,2", 'an allomorph';
    ok so(("a" | "b", 1).join(",") eq "a,1"), 'a Junction element threads';
}

is-deeply (a => 1).join("="), "a\t1", 'a Pair joins as its own Str, separator unused';

subtest 'Str on numbers', {
    plan 7;
    is-deeply 42.Str, "42", 'Int';
    is-deeply (2**70).Str, "1180591620717411303424", 'big Int';
    is-deeply 1e100.Str, "1e+100", 'Num';
    is-deeply (-0e0).Str, "-0", 'negative zero';
    is-deeply (1/3).Str, "0.333333", 'Rat';
    is-deeply FatRat.new(1, 3).Str, "0.333333", 'FatRat';
    is-deeply (1+2i).Str, "1+2i", 'Complex';
}

{
    my $s = "abc";
    ok $s.Str =:= $s.Str || $s.Str eq "abc", 'Str.Str is the string';
}

throws-like { (3/0).Str }, X::Numeric::DivideByZero, 'a zero-denominator Rat.Str throws';

{
    my $r = 1/4;
    is-deeply (^3).map({ $r.Str }).List, ("0.25" xx 3).List, 'Rat.Str through one site';
}

is-deeply (1..3).join("+"), "1+2+3", 'a Range joins its elements';

ok List.^can('join') && Rat.^can('Str'), 'introspection sees the methods';
