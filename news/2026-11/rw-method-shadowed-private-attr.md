# `$obj.acc = v` through an `is rw` method reaches a shadowed private attribute

When a parent and a child both declare a private `$!x`, each class keeps its
own copy (ADR-0121). Assigning through the parent's `method acc is rw { $!x }`
(`$b.acc = 7`) wrote the attribute under its bare name, which is neither
copy, so the write was lost. The store now goes to the copy of the class that
declares the method, with that declaration's type constraint and `Nil`
default, as in Rakudo (#18558).
