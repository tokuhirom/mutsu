use Test;

# An enum member answers the rows of the roles Rakudo composes into every enum
# type through its payload-kind shape, and its base type's rows on its value.
plan 17;

enum E <a b>;
is b.key, 'b', 'Enumeration.key';
is b.value, 1, 'Enumeration.value';
is b.pred, a, 'Enumeration.pred';
is b.Str, 'b', 'NumericEnumeration.Str is the key';
is b.Num, 1, 'an Int-valued member reads its base type row (Num)';
is b.is-prime, False, 'an Int row on the payload (is-prime)';
is b.^can('key').elems, 1, '.^can sees an enumeration row';
is b.^can('no-such-method').elems, 0, '.^can denies an unknown name';

enum S (pp => "x");
is pp.Str, 'x', 'StringyEnumeration.Str is the value';
is pp.key, 'pp', 'a Str-valued member has a key';
is pp.chars, 1, 'a Str row on the payload (chars)';

enum G (gg => 1.5);
is gg.key, 'gg', 'a generic-payload member has the Enumeration rows';

my Array enum A (:q[1, 2, 3]);
is q.elems, 3, 'an Array-valued member answers its base collection methods';

role Greet { method key { 'overridden' } }
enum R does Greet <r1 r2>;
is r1.key, 'overridden', 'a role the enum composes wins over the enumeration row';
is r2.value, 1, 'the other rows stay';
is E.enums.elems, 2, 'the type object answers enums';
is b.pairs.elems, 2, 'Any(Enumeration) views of the whole enumeration';
