# Cool's Unicode, case and indent methods with an argument are method-table rows

`uniprop`, `uniprops`, `unimatch`, `samecase`, `samemark` and `indent` with an argument now reach the
built-in method table's `Cool` rows for every `Cool` receiver: a `Range`, a `Seq`, an `IO::Path`, a
`Match`, an enum and an instance of a `Cool` subclass. An allomorph reads its string part
(`<65>.uniprop("Script")` is `Common`), and `indent(*)` goes through the row as well.

Receivers that are not `Cool`, such as a `Pair`, a `Capture`, a `Set`, a `Version` or a code
object, now raise `X::Method::NotFound` for these methods, as Rakudo does. Before, they quietly
answered on their string form.

With the rows in place, six more arms of the one-argument cascade are gone (180 → 174; ADR-11276
§9.90, part of #11276).
