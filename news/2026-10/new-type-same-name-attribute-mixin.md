# Attributes of two same-named `new_type` types no longer share a registry slot

`$attr does Role` on an Attribute records the mixed meta-object against `(owner, attribute
name)`. The owner was read back with `to_string_value()`, which cuts the storage key
`Q\0<digits>` of a second `Metamodel::ClassHOW.new_type(:name<Q>)` down to its display name
`Q`, so two distinct types filed their same-named attributes under one key and the second
type's `^attributes` returned the first type's attribute (type, role arguments and all).
Red's `^specialise` builds one such type per projection, so after `Model.^all.map(*.title)`
every later `.map(*.tags)` saw the first projection's column type and `Model.new` rejected
the inflated value with `X::TypeCheck::Assignment` (Red `t/03-blog`, `t/91-cross-product`).
The owner is now read through `attr_owner_key`, which keeps a `:package` type object's
storage name.
