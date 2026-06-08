use Test;

plan 9;

# `does` / `but` on a type-object invocant has no instance to mix into.
throws-like '(my $foo) does Int', X::Does::TypeObject,
    'does on an undefined scalar (Any type object) is illegal';
throws-like '(my $foo) does Int, Bool', X::Does::TypeObject,
    'does with a type-object list on a type object is illegal';
throws-like 'Bool does role { method Str() { $.raku } }', X::Does::TypeObject,
    'does on the Bool type object is illegal';
throws-like 'Int but role { method foo { 1 } }', X::Does::TypeObject,
    'but on the Int type object is illegal';

# Mixing a non-composable type (a class / type object, not a role or a
# concrete value) into a concrete object is illegal.
throws-like 'my $x = 5; $x does Int', X::Mixin::NotComposable,
    'does Int into a concrete Int is not composable';
throws-like 'my class NC { }; NC.new does NC', X::Mixin::NotComposable,
    :target(*.defined), :rolish(*.^name eq 'NC'),
    'does a class into an instance is not composable';
throws-like 'my class NC { }; NC.new but NC', X::Mixin::NotComposable,
    :target(*.defined), :rolish(*.^name eq 'NC'),
    'but a class into an instance is not composable';

# Valid mixins are unaffected.
{
    my $x = 5 but role { method foo { 99 } };
    is $x.foo, 99, 'mixing a role into a concrete value still works';
}
{
    my $y = 5 but "five";
    is ~$y, "five", 'mixing a concrete value still works';
}
