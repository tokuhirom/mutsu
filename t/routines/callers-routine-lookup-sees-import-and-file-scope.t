use v6;
use lib $?FILE.IO.parent(2).add('lib');
use Test;
use CallersScopeHub;
use CallersScopeProbe;

plan 8;

# A routine the importing unit got through a `sub EXPORT` hook is in its scope:
# `CALLERS::` finds it from the unit mainline, `CALLERS::OUTERS::` only from a
# routine, whose enclosing scope holds the unit's imports.
is-deeply probe(), (True, False), 'mainline caller: own pad has the import, no outer scope does';
sub inner { probe() }
is-deeply inner(), (True, True), 'routine caller: the unit scope around it has the import';


# A sub declared around the caller is in its lexical scope, also when the
# reading method has a CATCH phaser (the phaser's implicit try region is not an
# immediate block of its own).
is-deeply Seeker.new.ask, (True, True), 'a sub declared around the caller is in its lexical scope';
is-deeply Seeker.new.ask-with-catch, (True, True),
    'the same when the reading method has a CATCH phaser';

# Candidates of a built-in operator that a hook hands out add to the operator.
my $failure = Failure.new('x');
ok !$failure, 'prefix:<!> of a Failure falls back to the operator';
ok !$*undeclared-dynamic, 'prefix:<!> of an unresolved dynamic variable falls back too';
is !Leaf.new, 'leaf', 'the more specific hook candidate wins';
is !(class :: does Node {}).new, 'node', 'the role candidate takes a consumer';
