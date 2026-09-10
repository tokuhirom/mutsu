use Test;

plan 30;

# ---------------------------------------------------------------------------
# `enum NAME does Role BODY`
#
# The `does` clause used to be left unparsed: the `(...)`/`<...>` body was then
# read as a separate expression statement (warning "Useless use of '=>' in sink
# context") and the enum ended up with no values at all.
# ---------------------------------------------------------------------------

role Greeter { method greet { "hi" } }
enum Colour does Greeter <red green blue>;

is red.key,   'red',   'a does-composed enum still declares its values';
is green.value, 1,     'and their ordinals';
is red.^name, 'Colour', 'an enum value reports the enum type';
ok red ~~ Colour,       'an enum value type-checks as its enum';

is red.greet,    'hi', 'a composed role method is callable on an enum value';
is Colour.greet, 'hi', 'and on the enum type object';
ok red ~~ Greeter,      'an enum value does the composed role';
ok Colour ~~ Greeter,   'and so does the enum type object';

# `is` and `does` may repeat and appear in either order.
role A { method a { 'a' } }
role B { method b { 'b' } }
enum Multi does A does B <m1 m2>;
is m1.a ~ m1.b, 'ab', 'several `does` clauses all compose';
ok Multi ~~ A, 'the enum does the first role';
ok Multi ~~ B, 'the enum does the second role';

enum Ordered1 is export does A <o1 o2>;
enum Ordered2 does A is export <o3 o4>;
is o1.a, 'a', '`is export` before `does`';
is o3.a, 'a', '`does` before `is export`';

# A role's `ACCEPTS` override wins over the built-in enum smartmatch.
role Weird { multi method ACCEPTS(Int:D $v) { True } }
enum Flags does Weird (P => 1, Q => 2);
ok 5 ~~ P,          "a role's ACCEPTS override drives ~~ against an enum value";
ok 5 ~~ Flags,      'and against the enum type object';
ok P.ACCEPTS(5),    'the override is reachable as a plain method call too';
is P.value, 1,      'the pair-list body still assigns the declared values';

# A role mixed in at runtime supplies ACCEPTS the same way.
{
    my $matcher = 5 but Weird;
    ok 7 ~~ $matcher, "a mixed-in role's ACCEPTS drives ~~";
}

# A plain enum keeps the built-in Enumeration.ACCEPTS (compare by value).
enum Plain (Pa => 1, Pb => 2);
nok 5 ~~ Pa, 'a plain enum value does not match an unequal number';
ok  1 ~~ Pa, 'a plain enum value matches its own underlying value';

# ---------------------------------------------------------------------------
# Metamodel::EnumHOW introspection
# ---------------------------------------------------------------------------

enum Numbers <10 20>;
is Numbers.^elems, 2, '.^elems counts the enum values';
is-deeply Numbers.^enum_values, {"10" => 0, "20" => 1}, '.^enum_values maps name to value';
is Numbers.^enum_from_value(0), '10', '.^enum_from_value looks a value up';
is Numbers.^enum_from_value(1).^name, 'Numbers', 'and answers an enum value object';
is Numbers.^enum_from_value(99).raku, 'Mu', 'an unknown value answers Mu';
is Numbers.^enum_value_list.elems, 2, '.^enum_value_list lists the values';

enum Strung (sp => 'x', sq => 'y');
is-deeply Strung.^enum_values, {sp => 'x', sq => 'y'}, '.^enum_values of a Str-valued enum';
is Strung.^enum_from_value('x').key, 'sp', '.^enum_from_value on a Str-valued enum';
is Strung.^elems, 2, '.^elems of a Str-valued enum';

# `.^elems` on a non-enum metaobject used to abort the whole process with a Rust
# stack overflow (dispatch_elems_method <-> builtin_elems recursed forever). No
# `try` can survive that, so merely reaching the assertion is the regression pin.
# (raku rejects the call as an unresolvable caller; mutsu answers the inherited
# `Any.elems`. Either way it must terminate.)
{
    class Plainish {}
    lives-ok { try { Plainish.^elems } }, '.^elems on a class does not blow the stack';
}
