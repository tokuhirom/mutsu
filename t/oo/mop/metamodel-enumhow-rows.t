use Test;

# `Metamodel::EnumHOW`'s own metamethods and `WalkList`'s methods are rows of
# the method table (ADR-11276 §9.140). Answers checked against Rakudo.

plan 10;

enum Numbers <ten twenty>;
is-deeply Numbers.^enum_values, Map.new((ten => 0, twenty => 1)).Hash, 'enum_values';
is Numbers.^elems, 2, 'elems of an enum';
is Numbers.^enum_from_value(1), twenty, 'enum_from_value';
is-deeply Numbers.^enum_value_list.List, (ten, twenty), 'enum_value_list';
is Numbers.^is_composed, 1, 'a declared enum is composed';
{
    class C {}
    is C.HOW.elems, 1, 'elems of a ClassHOW is Any.elems';
    dies-ok { C.^enum_values }, 'enum_values is EnumHOW-only';
}

class A { method m { 1 } }
class B is A { method m { 2 } }
my $w = B.new.WALK("m");
is-deeply $w.invoke.List, (2, 1), 'WalkList.invoke';
is-deeply $w.reverse.invoke.List, (1, 2), 'WalkList.reverse';
is-deeply $w.quiet.invoke.List, (2, 1), 'WalkList.quiet';
