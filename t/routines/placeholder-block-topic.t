use Test;

plan 7;

# A bare block with placeholder params is still a *block*: it keeps the
# enclosing lexical `$_` (it does not reset the topic to Any the way a routine
# does), while the element binds to the placeholder parameter.
{
    $_ = "12345";
    is-deeply [3, 4].map({ "t=$_ a=$^a" }), ("t=12345 a=3", "t=12345 a=4"),
        'placeholder block inherits outer $_; element binds to $^a';
}

# Placeholder inside an immutable substitution replacement string (S/p/$^a/):
# the substitution runs on the inherited outer `$_`.
{
    $_ = "12345";
    is-deeply [3, 4].map({ S/5/$^a/ }), ("12343", "12344"),
        'placeholder in S/// replacement (// quoter)';
}

# Placeholder inside the substitution pattern.
{
    $_ = "12345";
    is-deeply [3, 4].map({ S/$^a/X/ }), ("12X45", "123X5"),
        'placeholder in S/// pattern (// quoter)';
}
{
    $_ = "12345";
    is-deeply [3, 4].map({ S{$^a} = 'X' }), ("12X45", "123X5"),
        'placeholder in S{} pattern ({} quoter)';
}

# Direct placeholder call keeps the caller's `$_`.
{
    $_ = "OUTER";
    my $b = { "t=[$_] a=$^a" };
    is $b(5), "t=[OUTER] a=5", 'direct placeholder block call keeps outer $_';
}

# A real `sub` does NOT inherit `$_` (topic is Any), even with placeholders.
{
    $_ = "OUTER";
    my $s = sub { defined($_) ?? "def" !! "undef" };
    is $s(), "undef", 'sub resets $_ to Any (not inherited)';
}

# A normal sub parameter is unaffected by the topic change.
{
    sub double($x) { $x * 2 }
    is double(21), 42, 'normal sub parameter still works';
}
