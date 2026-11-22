# RakuAST: a lone enum element is no list; an enum records its roles

`enum E (a=>3)` was viewed with its pair inside a one-operand
`ApplyListInfix`, so the `FatArrow` had no origin. The same happened to a lone
string (`enum G ('a')`). A lone colonpair (`enum K (:a(1))`) also lost its
spelling to a `FatArrow`. The parser now records a parenthesized body of one
element written without a comma as `EnumVariantForm::Lone`, with the element's
pair form. The view then gives the element itself, as Rakudo 2026.11 does. A
trailing comma (`(a=>3,)`) still makes the one-operand list.

Rakudo's `Type::Enum` also records the roles an enum composes, `Enumeration`
and `NumericEnumeration` or `StringyEnumeration`, as two `Trait::Does` nodes
after its written traits. `.traits` lists them, `visit-children` walks them
last, and `.raku` and `.clean-clone` (which its `Deparse` prints) leave them
out. mutsu now does the same whenever the base type is known from the source:
the `of` type, or the first value's literal type. A computed body gets none.
An enum also visits its term before its written traits, as Rakudo's does
(#18611, found by the `.origin` differential over `roast/S04-phasers/begin.t`,
#15780).
