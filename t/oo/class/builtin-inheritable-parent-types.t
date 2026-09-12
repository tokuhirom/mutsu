use Test;

# `class C is <CoreType> { }` for the core types mutsu models natively instead
# of registering a `ClassDef` for them. `validate_class_parents` vouched for a
# parent only through `registry.classes`, `BUILTIN_PARENT_TYPES`, the core-role
# oracle, `registry.roles` or `registry.enum_types` — so every core type absent
# from all five died as X::Inheritance::UnknownParent even though rakudo
# compiles the same declaration. `class ValueClass::Attribute is Attribute { }`
# is the shape that blocked the `ValueClass` distribution (and `Functional::
# Queue` / `Functional::Stack`, which depend on it) from loading at all;
# `is CX::Warn` blocked `Timezones::ZoneInfo` and `is Metamodel::SubsetHOW`
# blocked `Protocol`.
#
# Every name below was checked against rakudo 2026.07 with exactly this
# declaration; all of them compile there.

my @inheritable =
    'Attribute', 'CallFrame', 'CompUnit', 'CX::Return', 'CX::Warn', 'Cursor',
    'Deprecation', 'Duration', 'Instant', 'Label', 'NFC', 'NFD', 'NFKC',
    'NFKD', 'ObjAt', 'Scalar', 'StrDistance', 'Submethod', 'Uni',
    'Metamodel::ConcreteRoleHOW', 'Metamodel::CurriedRoleHOW',
    'Metamodel::EnumHOW', 'Metamodel::ModuleHOW', 'Metamodel::PackageHOW',
    'Metamodel::ParametricRoleGroupHOW', 'Metamodel::SubsetHOW';

plan @inheritable.elems + 5;

use MONKEY-SEE-NO-EVAL;

for @inheritable -> $parent {
    my $child = 'Child' ~ $++;
    lives-ok { EVAL "class $child is $parent \{ \}" },
        "class $child is $parent is a legal declaration";
}

# The declaration really produces a usable class, not just a silenced error.
class MyAttr is Attribute {
    method describe { 'described' }
}
is MyAttr.describe, 'described', 'a class inheriting a natively-modelled core type still works';
ok MyAttr.^name eq 'MyAttr', 'and keeps its own name';

# A genuine typo is still an unknown parent — the new list vouches for the
# names on it, nothing more.
throws-like 'class Oops is NoSuchParentType { }', X::Inheritance::UnknownParent,
    'an unknown parent is still X::Inheritance::UnknownParent';

# `does` is untouched: these are classes, not composable roles, so composing
# one is still rejected rather than silently accepted.
dies-ok { EVAL 'class Nope does Attribute { }' },
    'does Attribute is still rejected';

# And a `but`-mixin on an Attribute instance still takes the wrapper path
# (`types::role_mixin_class` keys that decision off BUILTIN_PARENT_TYPES,
# which this change deliberately left alone).
{
    role Tag { method tag { 'tagged' } }
    class Holder { has $.x }
    my $mixed = Holder.^attributes[0] but Tag;
    is $mixed.tag ~ ' ' ~ $mixed.name, 'tagged $!x',
        'mixing a role into an Attribute still keeps both halves';
}
