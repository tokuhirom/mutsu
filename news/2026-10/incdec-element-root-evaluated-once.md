# `++` on an element evaluates a non-variable root once

A nested `++`/`--` on an element reads the element and then writes it back,
and it compiled the element's container for each half. For a variable that is
only two lookups, but any other root ran twice: `f()[0]++` called `f` twice,
and `(constant w = [1, 2, 3])[0]++` redeclared `w`, which died with
"Redeclaration of symbol 'w'" at compile time. The compiler now binds such a
root to a temporary once and increments the same chain rooted at that
temporary; binding keeps the root's own container, so the write still lands in
it. Parentheses around a variable root (`(%h)<a><b>++`) are dropped the same
way; that increment used to be lost (#10582).

A method root (`$o.h<k>++`) keeps the old path for now: a write through a
bound alias of a typed-key hash attribute loses later writes made through the
accessor (#10803).
