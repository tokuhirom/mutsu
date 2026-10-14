use Test;

plan 12;

# A class-scoped public attribute (`my @.list`, `our $.elems`) gets an
# accessor method like any other, so it shadows the `Any` method of the same
# name -- on the type object and on an instance, through a variable or not
# (BDD::Behave's `my @.list of Failure` / `Failures.list.push(...)`).
class F { }
class C { my @.list of F; my $.elems = 4; my %.h; }

C.list.push(F.new);
is C.list.elems, 1, 'push through the type object reaches the array';
isa-ok C.list[0], F, 'the element is stored';
is C.elems, 4, 'a scalar class attribute named like a builtin';
is C.new.elems, 4, 'through a fresh instance';
my $c = C.new;
is $c.list.elems, 1, 'through an instance variable';

# Assigning through the accessor keeps the declared container type.
C.list = ();
is C.list.raku, 'Array[F].new()', 'assignment keeps Array[F]';
is C.list.elems, 0, 'and empties it';
C.list.push(F.new);
is C.list.elems, 1, 'still pushable after the assignment';
throws-like { C.list = (1,) }, X::TypeCheck::Assignment,
    'the element type is still enforced';

C.h = a => 1;
is-deeply C.h, {a => 1}, 'a hash attribute assigned a Pair';

class P { method list { 'user' } }
is P.list, 'user', 'a user method of that name still wins';
class Q { has @.list; }
is Q.new(list => (1, 2)).list.elems, 2, 'an instance attribute is unaffected';
