# A signature's type names resolve where the signature is written

A routine declared above a `my class DateTime is DateTime { }` in the same scope (the refining idiom of
`DateTime::strftime` and several other lizmat modules) used to have its `DateTime:D` parameters re-bound
to the later lexical class once the module was loaded, so every real `DateTime` was rejected.

The type-qualification pass now spells a type position that precedes a same-scope `my` type taking a
core type's name as `CORE::Name`, and `CORE::Name` constraints are matched against the registered core
type without any lexical, module or package alias. `DateTime::strftime` now passes all three test files
(72 of its 108 assertions never ran before).

Still open: an expression-position use (`where DateTime`) of such a name resolves to the later class.
