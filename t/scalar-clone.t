use Test;

# `.clone` is a Mu method available on every value. For an immutable value type
# (Int/Str/Num/Rat/Complex/Bool/Range/Version/Enum/...) it yields a copy, which
# is the value itself. These previously errored with "No such method 'clone'".

plan 16;

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
