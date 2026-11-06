# A nested class's method writes the outer class body's `my` lexical in place

A method of `class Outer::N` declared inside `class Outer { my %states; ... }` assigned `%states = ()`
into a copy: the write-back only looked in the current package's static store, while the reads
already walk the enclosing packages. The outer class's own methods and subs kept the old value.
Tinky::JSON's `state %states` shared by `from-json` and the attribute-trait callbacks made 2 of 26
tests of `t/020-construction.t` fail (#16460); the write now lands in the nearest enclosing package
that owns the lexical.
