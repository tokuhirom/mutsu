# A qualified type no longer matches an unrelated namesake

`class A {}; module Q { class A {} }` declares two unrelated classes, but
mutsu's type checks treated them as one: `A.new ~~ Q::A` was `True`, and a
`Q::A $x` parameter took an `A` (#14051). The static matcher's "qualified
name matching" bridge equated any `X::Foo` with a bare `Foo`. It existed to
paper over a resolution gap: a bare type name written inside a package
(`class C { role R {}; method m(R $x) {} }`) reached the run-time check
unresolved, as `R`, while the value's type was `C::R`.

The gap is now closed where it opens. A parse-time pass
(`parser::type_qualify`) qualifies a bare name in a type position, meaning a
parameter, a typed variable or attribute, or a return type, with the
innermost enclosing package that declares it. The package may declare the
name itself or through a `use`d module (`use URI::Path` inside `unit class
URI` makes `Path` mean `URI::Path`). `my`-scoped types keep their lexical
lookup. An imported parameterized role written bare (`Box[P]`) is checked
under its package alias. With both in place, the bridge is gone.

This is also the first step toward block-scoped type imports as pad
declarations (ADR-8751). `$x ~~ Foo` in a closure taken out of a `{ use M;
... }` block now rejects the program's own `Foo`.
