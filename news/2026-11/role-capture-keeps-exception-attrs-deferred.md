# `use Test` no longer declares every `X::Comp` class's attributes

Built-in exception classes declare their attributes on first use (#19092).
But registering a role that a built-in class composes, which loading `Test`
does for `X::Comp`, handed the role's captured scope to every composing
class's attributes. It walked each class's attribute list mutably, and that
declared the deferred attributes of about a hundred exception classes on
every load.

A table attribute's default is a literal and needs no captured scope, so the
walk now touches only the attributes a deferred class declared itself.

`use Test; ok 1;` went from 44.23M to 40.74M instructions (callgrind,
profiling build, warm cache).
