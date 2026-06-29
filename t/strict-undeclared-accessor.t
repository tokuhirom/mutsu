use Test;

plan 6;

# Loose-object-model strictness: a `.name` accessor on a USER-declared class
# resolves ONLY for a declared public `has $.name`. An undeclared name — e.g. an
# unknown named arg that `.new` accepted and stored — is NOT an accessor and
# throws X::Method::NotFound (Rakudo). Built-in/native objects, whose attributes
# live only in the stored map, are unaffected.

# Undeclared accessor on a user class throws.
my class Empty {}
throws-like { Empty.new(x => 3).x }, X::Method::NotFound,
    'undeclared .x on a user class throws X::Method::NotFound';

# A declared public attribute still works.
my class HasX { has $.x }
is HasX.new(x => 42).x, 42, 'declared public attribute accessor works';

# Undeclared name on a class that DOES declare other attributes also throws.
throws-like { HasX.new(x => 1, y => 2).y }, X::Method::NotFound,
    'undeclared .y throws even when the class declares other attributes';

# An attribute declared inside a nested sub/block is still registered.
my class NestedHas {
    sub helper { has $.deep }
}
is NestedHas.new(deep => 7).deep, 7, 'nested-scope has $.deep is registered';

# Built-in/native objects expose their attributes via the stored map (loose path
# intact): a type-check exception's .got/.expected are still accessible.
my $err;
{ my Int $a = "x"; CATCH { default { $err = $_ } } }
ok $err ~~ X::TypeCheck, 'caught a type-check exception';
is $err.got, "x", 'built-in X::TypeCheck .got still accessible';
