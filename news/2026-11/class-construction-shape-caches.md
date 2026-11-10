# Class construction memoizes three more per-class-shape lists

Constructing an object of a class with a `BUILD`/`TWEAK` walked the MRO and rebuilt
the same lists several times per call: the attribute type constraints (one cloned
`String` pair per attribute per phase), the attribute definitions, and the
attributes declared by more than one class of the hierarchy. All three depend on
the class shape alone, so they now come from the class's memoized
`NativeCtorPlan` / the type-stamp-checked `class_attributes_shared`, both already
dropped on every class-shape mutation. LLM::Chat's `Response.new` (about 40
attributes, a `Supplier`, a `Tap` and two `Lock`s) went from 0.73 to 0.52 ms and
its `emit` from 0.26 to 0.04 ms (rakudo: 0.06 and ~0). Construction is still several
times slower than rakudo's: a class with a `BUILD` submethod and two or more
attributes takes a generic method-lookup walk per `new` that a one-attribute class
does not (tracked in #17361).
