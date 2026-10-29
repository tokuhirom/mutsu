use Test;

# `.can` / `.^mro` on a built-in type name must not mistake it for a user class
# declared under a qualified name with the same tail, and a class declared later
# must still be found: the registry's short-name index is rebuilt after a write.
plan 9;

ok Int.can('Str'), '.can finds a built-in method on a built-in type';
nok Int.can('no-such-method'), '.can answers an empty list for a missing method';

class Outer::Int { method only-outer { 1 } }
class Outer::Inner is Outer::Int { method hello { 2 } }

ok Outer::Inner.can('hello'), '.can finds a method of a qualified user class';
ok Outer::Inner.can('only-outer'), '.can finds an inherited method of a qualified class';
nok Int.can('only-outer'), "a qualified class's method does not leak into the built-in Int";
nok Any.can('hello'), "nor into Any";

is-deeply Int.^mro.map(*.^name).head(1).List, ('Int',), '.^mro of a built-in still starts with itself';

grammar G::Sub { token TOP { 'a' } }
ok G::Sub.parse('a'), 'a grammar under a qualified name still parses';
is G::Sub.^mro.map(*.^name).grep('Grammar').elems, 1, 'and still inherits Grammar once';
