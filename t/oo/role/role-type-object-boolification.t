use Test;

# A role's type object boolifies through Mu (False), never through a `Bool`
# method the role declares: rakudo's role punning excepts `Bool`. Red's
# `if $ast.?filter` on the `Red::AST` type object relies on it, and the answer
# must not depend on whether something punned the role earlier (on any
# thread). Each expectation matches `raku`.

plan 6;

role R { method Bool { True }; method hello { 'hi' } }

nok so R, 'so R is False before the role is punned';
nok R.Bool, 'R.Bool is Mu.Bool on the type object';
is R.hello, 'hi', 'a role method is reached through the pun';
nok so R, 'so R is still False after the role was punned';
my $x = R;
my $branch = $x ?? 'true' !! 'false';
is $branch, 'false', 'a role type object in a variable is false in a condition';
nok (await start { so R }), 'so R is False on another thread too';
