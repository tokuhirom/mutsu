use Test;

plan 3;

# `constant = EXPR` is an anonymous constant declaration (Language/terms.rakudoc).
# Its initializer runs at BEGIN time (ADR-0134), so `$ran` is declared without a
# run-time initializer that would overwrite what it stores.
my $ran;
constant = ($ran = 1);
is $ran, 1, 'anonymous constant initializer is evaluated';

constant = "anon";
constant = 42;
pass 'several anonymous constants do not collide';

throws-like 'constant foo;', X::Syntax::Missing, 'a named constant still needs an initializer';
