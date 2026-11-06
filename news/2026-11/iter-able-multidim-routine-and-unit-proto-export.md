# Iter::Able: `postcircumfix:<[; ]>` routine, bodied unit-module proto exports, Seq list slices

Four gaps found running Iter::Able's own suite next to rakudo:

- `postcircumfix:<[; ]>` is now a CORE routine, callable by name and as `&postcircumfix:<[; ]>`
  (`flatten` calls it with a list of Whatevers). It drives the same multi-dimensional index opcode
  as the `@a[*;*]` syntax.
- A `proto` with its own body and no candidates, exported from a `unit module` whose name differs
  from its file (`Iter::Able::Select-At` declares `unit module Select-At`), is now imported; the
  unit-module export table is also recorded under the requested compunit path.
- A list slice of a Seq made from an Array (`[5, 6, 7].Seq[0, 2]`) answered `Nil`; it now answers
  the elements.
- A built-in iterator's `is-lazy` is its own flag even when the program composes the `Iterator`
  role (the role's `False` default had replaced it), so `[1, 2].lazy.iterator.is-lazy` is `True`.

Iter::Able's t/12, t/14, t/22, t/23 and t/30 now match rakudo; the rest of that suite is tracked in
the follow-up issues.
