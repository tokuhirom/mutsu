use Test;

plan 3;

# `constant = EXPR` is an anonymous constant declaration (Language/terms.rakudoc).
my $ran = 0;
constant = ($ran = 1);
is $ran, 1, 'anonymous constant initializer is evaluated';

constant = "anon";
constant = 42;
pass 'several anonymous constants do not collide';

throws-like 'constant foo;', X::Syntax::Missing, 'a named constant still needs an initializer';
