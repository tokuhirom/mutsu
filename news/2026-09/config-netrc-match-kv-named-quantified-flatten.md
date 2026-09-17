# Config::Netrc: Match.kv/.values now flatten quantified named captures

`Config::Netrc` (ecosystem, drawn via roulette lock board #7884) died on its
own test suite with "Type Array does not support associative indexing.",
while rakudo passed cleanly (5/5).

The module's grammar has a repeated top-level alternation of named subrules,
`token TOP { ^ [<line-comment>|<entry>|<eol>]*? $ }`. Because the alternation
is quantified, each name's submatches are stored as an Array on the
resulting Match — even a name matched zero or one time. The module's action
code then does `for $/.kv -> $elem { if $elem<name>.defined { ... } }`.

In raku, `Match.kv` flattens a quantified/multi-match named capture's Array
into the surrounding sequence: a name matched twice contributes its key
followed by both Match objects individually, a name matched once
contributes its key followed by the bare Match (not a one-element Array),
and a name matched zero times contributes only its bare key with nothing
following. mutsu already had this flattening for the *positional* branch of
`.kv` (`$0` from `(x)*`) but pushed the raw Array whole for the *named*
branch — so a caller iterating `$/.kv` got a bare Array as one of the loop
elements, and indexing it (`$elem<name>`) threw "Type Array does not
support associative indexing." `.values` had the identical asymmetry.
`.pairs` and direct `<name>` access were already correct on both sides —
those keep a quantified capture's Array as a single value in both mutsu and
raku, confirmed by a direct comparison against `raku`.

Fixed in `src/builtins/methods_0arg/mod.rs`'s Match `"kv"`/`"values"`
handlers: the named-capture branch now flattens a quantified/multi-match
Array exactly like the existing positional branch already did, rather than
pushing it as one value. Pinned by
`t/regex/match/match-named-capture-kv-values-flatten.t`, checked against
`raku` first.

`Config::Netrc` moves from `red` (0/1 baseline files) to `green` (1/1 files,
5/5 assertions), matching rakudo. No issues filed — nothing left red.
