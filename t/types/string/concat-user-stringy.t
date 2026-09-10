use Test;
plan 8;

# Infix ~ stringifies an operand via .Stringy (falling back to .Str).
{
    class S1 { method Str { "Z" } }
    is (S1.new ~ "!"), "Z!", 'infix ~ dispatches user .Str (right operand literal)';
    is ("[" ~ S1.new), "[Z", 'infix ~ dispatches user .Str (left operand literal)';
}

# .Stringy is preferred over .Str for infix ~ (Raku behavior).
{
    class S2 { method Str { "STR" }; method Stringy { "STRINGY" } }
    is ("a" ~ S2.new ~ "b"), "aSTRINGYb", 'infix ~ prefers .Stringy over .Str';
}

# .Str is used when there is no .Stringy.
{
    class T { method Str { "T!" } }
    is (T.new ~ T.new), "T!T!", 'both operands dispatch user .Str';
}

# Captured-outer write inside the stringifier propagates.
{
    my $calls = 0;
    class C { method Str { $calls++; "x" } }
    my $c = C.new;
    my $a = $c ~ "";
    my $b = $c ~ "";
    ok $calls >= 2, 'user .Str captured-outer write propagates across ~';
}

# Plain values are unaffected.
{
    is ("a" ~ "b" ~ 3), "ab3", 'plain Str/Int concat unaffected';
    is (1 ~ 2), "12", 'Int ~ Int unaffected';
}

# A class without a stringifier still renders its gist (unchanged).
{
    class P { }
    ok (P.new ~ "").contains("P"), 'no-stringifier class renders gist';
}
