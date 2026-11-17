# A grammar cursor's Match methods answer from the method table

A plain regex match has the `Match` dispatch shape, so `.ast`, `.from` or
`.Str` on it is answered straight from the method table. A grammar cursor
reports its grammar's own class, which may declare any `Match` method
itself, so it had no shape. Every such call, such as `$<x>.ast` in each
action body, ran the whole method-call probe chain: ~16k instructions,
against ~1k on a plain match.

The method-site lane now answers a cursor as a `Match` when the grammar's
MRO declares no user method of that name and `Match` was not `augment`ed with
it. The verdict is memoized per (grammar class, method) for one registry
generation. A grammar that does declare the method, directly or through a
parent grammar, still gets its own method. On `Cro::Uri.parse-ref`,
together with the pre-sized attribute map, 600 parses went from 1,251M to
1,209M instructions.

`Match.raku` also now lists `:made(...)` when the match has a `make` value,
as Rakudo does.
