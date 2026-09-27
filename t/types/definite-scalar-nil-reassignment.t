use Test;

plan 8;

# #9779: a Nil ASSIGNED to an already-declared `:D`-constrained scalar is a
# genuine failing assignment (X::TypeCheck::Assignment), not a missing
# initializer (X::Syntax::Variable::MissingInitializer) -- that error is
# reserved for a `my Foo $x;` that never wrote an initializer at all.

my Int:D $i = 1;
throws-like { $i = Nil }, X::TypeCheck::Assignment,
    symbol => '$i',
    'reassigning Nil to an already-declared Int:D scalar';
is $i, 1, 'the rejected reassignment leaves the scalar untouched';

sub returns-nil(--> Int:D) { Nil }

throws-like { my Int:D $j = returns-nil() }, X::TypeCheck::Assignment,
    symbol => '$j',
    'a declaration whose explicit initializer evaluates to Nil at runtime';

my Int:D $k = 1;
throws-like { $k = returns-nil() }, X::TypeCheck::Assignment,
    symbol => '$k',
    'reassigning from a call that returns Nil at runtime';
is $k, 1, 'the rejected call-returning-Nil reassignment leaves the scalar untouched';

# A bare declaration with NO initializer at all is still MissingInitializer.
throws-like 'my Int:D $m;', X::Syntax::Variable::MissingInitializer,
    type => 'Int:D',
    'a declaration with no initializer at all is still MissingInitializer';

# A literal `Nil` initializer is a genuine (failing) assignment too.
throws-like { my Int:D $n = Nil }, X::TypeCheck::Assignment,
    symbol => '$n',
    'a literal Nil initializer is a failing assignment, not MissingInitializer';

# Sanity: ordinary typed reassignment still works.
my Int:D $ok = 5;
$ok = 10;
is $ok, 10, 'an ordinary definite reassignment still works';
