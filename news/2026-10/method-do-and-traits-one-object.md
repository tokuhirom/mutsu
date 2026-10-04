# A method's `$!do` is its body; its traits share one Method object

Following ADR-11827 phase 2, every trait of a method declaration now
receives the same `Method` object. A role one trait composes is visible to
the next, and the object carries the declared return type, so `.returns` and
`.signature.returns` answer it. Binding `$!do` on a method code object (from
a trait handler, or on a `Method` that `.^find_method` returned) replaces
the body dispatch runs for that candidate. The bound body is called with the
invocant first, and `.wrap` wrappers still run around it. This is the path
upstream NativeCall's `method m(...) is native` takes.

Two introspection fixes came with it. `Parameter.type` is now the nominal
type: for `Mem:D $x` it is `Mem`, and `:D` is reported by `.modifier`, as in
Rakudo. A return type spelled with an imported type's short name (`size_t`
under `use NativeCall`) answers that module's type object, which is what the
bare term evaluates to.
