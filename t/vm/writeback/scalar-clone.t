use Test;

# `.clone` is a Mu method available on every value. For an immutable value type
# (Int/Str/Num/Rat/Complex/Bool/Range/Version/Enum/...) it yields a copy, which
# is the value itself. These previously errored with "No such method 'clone'".

plan 21;

is 5.clone, 5, 'Int.clone';
is (2 ** 70).clone, 2 ** 70, 'BigInt.clone';
is "abc".clone, "abc", 'Str.clone';
is 3.14e0.clone, 3.14e0, 'Num.clone';
is (1/2).clone, 0.5, 'Rat.clone';
ok (3+4i).clone == 3+4i, 'Complex.clone';
is True.clone, True, 'Bool.clone (True)';
is False.clone, False, 'Bool.clone (False)';

is (1..5).clone.raku, (1..5).raku, 'Range.clone';
is (1..^5).clone.raku, (1..^5).raku, 'RangeExcl.clone';
is ('a'..'e').clone.join, 'abcde', 'GenericRange.clone';

is 42.clone.WHAT.^name, 'Int', 'Int.clone preserves type';
is (1/3).clone.WHAT.^name, 'Rat', 'Rat.clone preserves type';

{
    enum Color <red green blue>;
    is green.clone, green, 'Enum value .clone';
    ok green.clone == green, 'Enum value .clone numeric identity';
}

# Regression: an object's `.clone(:attr(v))` (named-arg attribute override) still
# routes through the slow-path handler.
{
    class P { has $.x; has $.y; }
    my $q = P.new(x => 1, y => 2).clone(y => 9);
    is "{$q.x} {$q.y}", '1 9', 'Instance.clone(:named) override unaffected';
}

# Regression guard: scalar `.clone` must NOT hijack the delegation paths that
# clone a Match or a mixed-in value (which coerce to their inner scalar for
# unknown methods). Adding scalar `.clone` initially broke both.

# A Match clones to a Match, preserving its named captures (roast S12/clone.t).
{
    my $p = 'a' ~~ /$<foo>='a'/;
    my $q = $p.clone;
    is $q.^name, 'Match', 'Match.clone stays a Match';
    is ~$q<foo>, 'a', 'Match.clone keeps the named capture';
}

# A `but`-mixed value clones preserving the mixin.
{
    my $x = 5 but False;
    is $x.clone.Bool, False, 'mixin (but False) preserved through clone';
    is $x.clone + 0, 5, 'mixin clone keeps the inner value';
}

# An allomorph clones to the same allomorph type.
{
    my $a = <42>;
    is $a.clone.WHAT.^name, 'IntStr', 'allomorph clone keeps the allomorph type';
}
