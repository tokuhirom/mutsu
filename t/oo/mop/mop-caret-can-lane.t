use Test;

# `.^can(NAME)` on instances and type objects of ordinary classes, on a user
# metaclass instance, and on a class whose `.HOW` was already asked for (the
# cached default metaobject is not a custom one).

plan 14;

class MyHOW is Metamodel::ClassHOW { method as(Mu \t) { "x" } }
my $how = MyHOW.new;
my $type = $how.new_type(:name<Foo>);
$how.compose($type);

is $type.HOW.^can("as").elems, 1, 'a metaclass instance finds its own method';
is $type.HOW.^can("compose").elems, 1, 'and one inherited from ClassHOW';
is $type.HOW.^can("nope").elems, 0, 'and not a missing one';
is $how.^can("as")[0].name, 'as', 'the Method object is the declared one';

class C { method m { } }
class D is C { method m { } }
is D.^can("m").elems, 2, 'type object: every candidate along the MRO';
is D.new.^can("m").elems, 2, 'instance: the same';
is D.^can("new").elems, 1, 'a Mu method';
is C.^can("m")[0].package.^name, 'C', 'the owner is the declaring class';
my $m = "m";
is D.^can($m).elems, 2, 'a name in a variable';
ok 5.^can("Str").elems > 0, 'a built-in value';

# Asking `.HOW` of a class first must not hide its methods from `.^can`.
class E { has $.x; method e { } }
my $e-how = E.HOW;
is $e-how.^name, 'Perl6::Metamodel::ClassHOW', '.HOW of a plain class';
is E.^can("e").elems, 1, '.^can after .HOW was asked';
is E.^name, 'E', '.^name after .HOW was asked';

class WithCan { method ^can(Mu \t, $name) { ("mine",) } }
is WithCan.^can("anything")[0], 'mine', 'a class declaring ^can keeps it';
