# Reflect parametric container roles through `.of`

Container subclasses that statically compose `Associative[V,K]` or
`Positional[V]` now report `V` from `.of`. The lookup reads the parameterized
role spelling retained in class composition metadata, including roles composed
by a nominal ancestor.

This makes `class DateHash is Hash does Associative[Cool, DateTime]` report
`Cool` through both a `%`-bound value and an ordinary instance. The same
behavior is pinned for Array/Positional subclasses and inherited composition.
