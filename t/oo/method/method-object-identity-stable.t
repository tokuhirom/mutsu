use Test;
use nqp;

# A method's `Method` object keeps its identity for the life of its class,
# however often the metaclass's lookup caches are rebuilt in between (an
# `EVAL` that declares something clears them). static keys its table of
# unit-declared methods by `nqp::objectid` of the declaration's
# `compile-time-value` and finds them again through `.^find_method` (#18107).

plan 7;

class P { method p() { 1 } }
class Q is P { }
role R { method r() { } }
class A does R { }
class B does R { }

my $before = P.^find_method('p');
EVAL 'sub declared-in-between() { }';
EVAL 'class DeclaredInBetween { }';
my $after = P.^find_method('p');
ok $before === $after, 'find_method answers the same object after the caches were rebuilt';
is nqp::objectid($before), nqp::objectid($after), '... with the same objectid';
ok $after.candidates[0] === $before, 'a plain method is its own candidate';
ok P.^lookup('p') === $before, '.^lookup answers it too';
ok P.^methods.first(*.name eq 'p') === $before, '.^methods too';
ok Q.^find_method('p') === $before, 'an inherited method is the same object';
nok A.^find_method('r') === B.^find_method('r'),
    'a role method composed into two classes is two methods';
