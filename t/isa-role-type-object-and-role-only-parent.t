use v6;
use Test;

plan 14;

# `.isa` answers *class* inheritance. Two shapes of it were wrong in mutsu, both
# reachable from ordinary "is this container still undefined?" code:
#
#  1. A ROLE TYPE OBJECT has no class MRO of its own, so `class_mro` on the role
#     name yielded just the name and every probe answered False. raku answers
#     from the chain the role's pun would have -- exactly `Any`, then `Mu`.
#     `my Associative $t; $t.isa(Any)` (Crane's "original container is
#     unchanged" assertion) therefore reported False.
#
#  2. A class whose only declared parents are COMPOSED ROLES (`class K does A`)
#     linearized to `(K, A)` and stopped there, so `K.isa(Any)` was False even
#     though `K.^mro` listed `Any` and `Mu`.

role A { }
role B does A { }
class K does A { }
class Plain { }
class Sub is Plain { }

# 1. role type objects
ok Associative.isa(Any), 'a built-in role type object isa Any';
ok Associative.isa(Mu), 'a built-in role type object isa Mu';
nok Associative.isa(Cool), 'but not an unrelated class';
nok Associative.isa(Str), 'nor Str';
ok Positional.isa(Any), 'Positional isa Any too';
ok A.isa(Any), 'a user role type object isa Any';
ok B.isa(Mu), 'including one that composes another role';

# An undefined scalar typed with a role holds that role's type object.
my Associative $t;
ok $t.isa(Any), 'an undefined Associative-typed scalar isa Any';
my Positional $q;
ok $q.isa(Any), 'an undefined Positional-typed scalar isa Any';

# 2. classes whose only parents are roles
ok K.isa(Any), 'a class whose only parent is a role still isa Any';
ok K.isa(Mu), 'and isa Mu';
ok K.new.isa(Any), 'and so does its instance';

# Ordinary class inheritance is untouched.
ok Sub.isa(Plain), 'a subclass isa its parent';
ok Sub.isa(Any), 'and isa Any';
