# Constructor binding retains the supplied container

`has $.value is built(:bind)` now binds the caller's container instead of
assigning its value into a fresh attribute slot. Writes through the attribute
and the caller observe the same scalar or element cell. A literal binds as an
immutable value. Bound defaults return their container too, including defaults
applied after BUILD.

The native constructor, generic construction, `bless` forwarding and replaced
BUILDPLAN tasks share the binding operation. Ordinary attributes still assign
into their own storage, and native attributes keep their assignment semantics.
Named arguments retain their containers until the receiving binding decides
whether to alias or take a value; slipping a named-argument hash preserves them.
Native method rows bind ordinary named parameters by value, while factory
relays declare that they preserve a raw capture for the constructor's binder.
RakuAST value constructors follow the same boundary.

This addresses #14559. The regression compares scalar and element sources,
immutable values, defaults, BUILD ownership, inheritance and constructor caches
against Rakudo.
