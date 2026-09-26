use Test;

# A role's method stub is satisfied by name by any method the class declares or
# inherits -- including a `token`/`regex`/`rule` and a `proto method`, which
# mutsu stores outside the ordinary method table. CSS::Module's grammars
# implement their `Gen::External` role's `method number (|) {...}` with a
# `token number`, and its actions a `method length (|) {...}` with an inherited
# `proto method length {*}`.

plan 4;

role Ext { method number(|) { ... } }
grammar P { token number { \d+ }; token TOP { <number> } }
grammar H is P does Ext { }
is ~H.parse('43')<number>, '43', 'an inherited token satisfies the stub';

role BaseG { token number { \d+ } }
grammar G0 { token TOP { <number> } }
grammar G is G0 is BaseG does Ext { }
is ~G.parse('42')<number>, '42', 'a token from a punned role parent satisfies the stub';

role Len { method length(|) { ... } }
class PL { proto method length(|) {*}; multi method length(Int $x) { 'int' } }
class A is PL does Len { }
is A.new.length(1), 'int', 'an inherited proto method satisfies the stub';

class B does Len { proto method length(|) {*}; multi method length(Str $x) { 'str' } }
is B.new.length('x'), 'str', 'an own proto method satisfies the stub';
