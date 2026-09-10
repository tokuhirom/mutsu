use Test;

plan 11;

# Applying `but` to a role type object carries a ParametricRoleGroupHOW, which
# has no `mixin` metamethod, so raku reports X::Method::NotFound (NOT
# X::Does::TypeObject). The exception must carry a non-empty message.
throws-like 'my \foo = Callable but role :: { }',
    X::Method::NotFound, :message{.so},
    'but on the Callable role type object throws X::Method::NotFound';

throws-like 'my \x = Iterable but role :: { }',
    X::Method::NotFound, method => 'mixin',
    'but on a built-in role (Iterable) throws X::Method::NotFound(mixin)';

throws-like 'my \x = Numeric but role :: { }',
    X::Method::NotFound,
    'but on a built-in role (Numeric) throws X::Method::NotFound';

throws-like 'role R { }; my \x = R but role :: { }',
    X::Method::NotFound, method => 'mixin',
    'but on a user-declared role type object throws X::Method::NotFound(mixin)';

# `does` on any type object (role or class) stays X::Does::TypeObject —
# this change is scoped to `but`.
throws-like 'Callable does role :: { }',
    X::Does::TypeObject,
    'does on a role type object stays X::Does::TypeObject';

# `but` on a non-role (class) type object creates a mixin type object
# (`Int+{R}`), including parametric type objects like Array[Int].
{
    my \x = Int but role { method answer { 42 } };
    is x.answer, 42, 'but on a class type object creates a mixin type object';
    nok x.defined, 'the mixin type object is undefined';
}
{
    my role Tagged { method tag { "tagged" } }
    my constant T = Array[Int] but Tagged;
    is T.^name, 'Array[Int]+{Tagged}',
        'parametric base name is preserved in ^name';
    my $a = T.new;
    $a.push(5);
    is $a.tag, 'tagged', 'instances made from the mixin type do the role';
    is $a[0], 5, 'and keep the base type behavior';
}

# `but` mixing a concrete value into a type object is unaffected.
lives-ok { my \x = Int but True; }, 'but with a concrete value still works';
