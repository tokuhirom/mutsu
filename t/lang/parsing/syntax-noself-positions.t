use Test;

# The X::Syntax::NoSelf checks walk every position of a `self`-less body with
# the typed AST visitor (ADR-0137), so an attribute named through its twigil is
# rejected wherever rakudo rejects it -- not only in the forms a hand-rolled
# walker happened to list. Each expectation below was checked against rakudo.

plan 17;

# A `$!attr` in a plain sub declared in a class body.
for (
    'class C1 { has $.a; sub f { say [$!a] } }',
    'class C2 { has $.a; sub f { my %h = (x => $!a) } }',
    'class C3 { has $.a; sub f { if 1 { $!a } } }',
    'class C4 { has $.a; sub f { for 1 { say $!a } } }',
    'class C5 { has $.a; sub f(:$x = $!a) { } }',
    'class C6 { has $.a; sub f { say $!a.Str } }',
    'class C7 { has $.a; sub f { $!a = 1 } }',
    'class C8 { has $.a where { (a => $!a) } }',
) -> $code {
    throws-like $code, X::Syntax::NoSelf, "rejected: $code";
}

# A nested method -- declared or literal -- brings its own `self`.
for (
    'class D1 { has $.a; sub f { my method m { $!a } } }; 1',
    'class D2 { has $.a; sub f { my $m = method { $!a } } }; 1',
    'class D3 { has $.a; sub f { my $m = anon method foo { $!a } } }; 1',
    'class D4 { has $.a; sub f { class E { has $.b; method m { $!b } } } }; 1',
) -> $code {
    lives-ok { EVAL $code }, "accepted: $code";
}

# The no-twigil alias of `has $x` read at class-body level.
for (
    'class F1 { has $x; say ($x, 1) }',
    'class F2 { has $x; my @a = $x xx 2 }',
    'class F3 { has $x; my $y = $x }',
) -> $code {
    throws-like $code, X::Syntax::NoSelf, "rejected: $code";
}

# A nested block may declare a lexical of the same name.
for (
    'class G1 { has $x; my $y = { my $x = 1; $x } }; 1',
    'class G2 { has $x; my $y = -> $x { $x } }; 1',
) -> $code {
    lives-ok { EVAL $code }, "accepted: $code";
}
