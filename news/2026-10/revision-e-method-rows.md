# Methods of language revision 6.e exist only under 6.e

The method table knows which methods Rakudo declares only in its `core.e`
setting (ADR-11276 §9.146). `Any.snip`, `Any.snitch`, `Supply.snip`,
`Cool.nomark`, `Str.nomark` and `IO::Path.stem` are rows flagged
`REVISION_E`: under `use v6.e.PREVIEW` they answer and `.^can` finds them,
and under 6.d the call is `X::Method::NotFound` and `.^can` is false, as in
Rakudo. Before, `snip` answered under 6.d too, `.^can` denied all of them
under 6.e, and `nomark` and `stem` did not exist. The Rakudo method-table
snapshot gains the names 6.e adds (`declared.e`/`methods.e`/`only.e`), and the
oracle test checks every `REVISION_E` row against them.
