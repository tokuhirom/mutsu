use Test;

# A *named* `anon sub` in expression context (notably inside parentheses) used
# to fail to parse: `(anon sub jp($p) { ... })` gave a SORRY, while the same
# without parentheses (`my $x = anon sub jp($p) {...}`) or without a name
# (`(anon sub ($p) {...})`) parsed fine. The `anon sub` handler only recognized
# `anon sub {...}` and `anon sub (...)`, never `anon sub NAME ...`.
# (Needle::Compile self-installs a JSON::Path stub this way.)

plan 4;

# Parenthesized named anon sub, assigned then called.
{
    my $x = (anon sub jp($p) { $p ~ "!" });
    is $x("hi"), "hi!", "parenthesized named anon sub, scalar binding";
}

# Directly called named anon sub in parens (no assignment).
{
    my $out;
    (anon sub greet($n) { $out = "hello $n" })("world");
    is $out, "hello world", "directly-called named anon sub in parens";
}

# Named anon sub with a typed parameter.
{
    my $x = (anon sub jp(Str $p) { "got: $p" });
    is $x("yo"), "got: yo", "named anon sub with typed param";
}

# Named anon sub with a block body only (no signature).
{
    my $y = anon sub named2 { 42 };
    is $y(), 42, "named anon sub with block body only";
}
