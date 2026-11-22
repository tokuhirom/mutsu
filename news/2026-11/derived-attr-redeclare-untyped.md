# A derived class's redeclared attribute does not take the base's type

`class Derived is Base { has @.label is rw }` over `class Base { has Str
$.label }` made `Derived.new.label` an `Array[Str]`: the attribute type
lookups (`get_attr_type_constraint` and the constructor's per-MRO constraint
table) took the first *typed* declaration of the name along the MRO, so the
base's `Str` typed the derived, untyped `@.label`. Both now stop at the
most-derived declaration of the name, typed or not, which is the attribute
the bare name stores, as in rakudo (#17994).
