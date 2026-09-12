# A class in a role body inheriting Attribute was already fixed

[#8062](https://github.com/tokuhirom/mutsu/issues/8062) reported that
`class MyAttr is Attribute { }` died as `X::Inheritance::UnknownParent`,
blocking the PDF distribution family (`PDF`, `PDF::Class`,
`PDF::Content`, `PDF::Font::Loader`, `FDF`, `Pod::To::PDF::Lite`) via
`PDF::COS::Tie`'s attribute-trait helper classes, declared inside a role
body.

`Attribute` was added to `BUILTIN_INHERITABLE_TYPES` by an unrelated,
broader fix (`af7e5ab3`, "an uppercase `is Trait` is still a trait, and a
natively-modelled core type is still a parent", closing
[#7996](https://github.com/tokuhirom/mutsu/issues/7996)) shortly before
this ticket was picked up — both the top-level case and the role-body
case measure correctly against `raku` now.

Extended `t/oo/class/builtin-inheritable-parent-types.t` with the
role-body shape specifically (a `my class ... is Attribute` declared
inside a `role`, composed into a consumer class to make it reachable) so
that this coverage is not left implicit in the top-level case alone.
