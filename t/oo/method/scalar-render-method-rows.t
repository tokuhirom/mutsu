use Test;

# ADR-11276: gist, raku and WHICH on the scalar types are method-table rows.

plan 33;

is 5.gist, "5", "Int.gist";
is 5.raku, "5", "Int.raku";
is (10**30).gist, "1000000000000000000000000000000", "big Int.gist";
is 1.5e0.gist, "1.5", "Num.gist";
is 1.5e0.raku, "1.5e0", "Num.raku";
is (1/3).gist, "0.333333", "Rat.gist";
is (1/3).raku, "<1/3>", "Rat.raku";
is (6/3).raku, "2.0", "whole Rat.raku";
is (1.5).raku, "1.5", "decimal Rat.raku";
is (1+2i).gist, "1+2i", "Complex.gist";
is (1+2i).raku, "<1+2i>", "Complex.raku";
is True.gist, "True", "Bool.gist";
is False.raku, "Bool::False", "Bool.raku";
is True.Str, "True", "Bool.Str";
is "a b".gist, "a b", "Str.gist";
is "a\nb".raku, '"a\nb"', "Str.raku escapes";
is "q\"".raku, '"q\""', "Str.raku quotes";

is 5.WHICH, "Int|5", "Int.WHICH";
is "a".WHICH, "Str|a", "Str.WHICH";
is 2.5e0.WHICH, "Num|2.5", "Num.WHICH";
is (1/3).WHICH, "Rat|1/3", "Rat.WHICH";
is (1+2i).WHICH, "Complex|1|2", "Complex.WHICH";
is (1.5+2.5i).WHICH, "Complex|1.5|2.5", "Complex.WHICH with fractional parts";
ok (1+2i).WHICH eq (1+2i).WHICH, "equal Complexes share an identity";
isa-ok 5.WHICH, ObjAt, "Int.WHICH is an ObjAt";
isa-ok "a".WHICH, ValueObjAt, "Str.WHICH is a ValueObjAt";
ok 5.WHICH === 5.WHICH, "equal Ints share an identity";
ok "a".WHICH eq "a".WHICH, "equal Strs share an identity";

# Zero-denominator rationals keep their errors.
throws-like { (1/0).gist }, X::Numeric::DivideByZero, "Rat.gist of x/0 throws";
is <1/0>.raku, "<1/0>", "Rat.raku of x/0";

# A type object and a mixin do not take the rows.
is Int.gist, "(Int)", "Int type object gist";
is Str.raku, "Str", "Str type object raku";
is <1e3>.gist, "1e3", "an allomorph gists as its source";
