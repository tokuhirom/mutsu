# A `new_type` minted under an existing type's name keeps its own stash members

A type minted by `Metamodel::*HOW.new_type(:name(X))` under a name another type
already holds is a different type that merely shares a display name; internally
it gets a mangled storage name (`X␀<id>`). Binding a member into its `.WHO`
stash used to clobber the same-named member of the declared type `X`: the
mangled key's member was mirrored under the de-mangled, user-facing name into
the shared package store (a mechanism meant for a lexical class's `my class X {
our $v }`, #16620), so `X::member` ended up holding the minted type's value.

Red's `MetamodelX::Red::Specialisable` hits this: it mints a projection model
named after the source model and binds `type.WHO<ResultSeq> := Proxy.new(...)`
on it, which overwrote the declared model's own `ResultSeq`. The facing-name
mirror is now suppressed for a minted-type segment (`is_minted_type_name`,
#14711), and a `new_type` over a name a custom-HOW type already declared
(registered in `class_how_values` before it composes) now mangles too. Red's
t/104 subtests 1–5 match `raku`; the remaining concurrent subtest is #17697.
