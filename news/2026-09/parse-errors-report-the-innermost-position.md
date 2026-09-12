# Parse errors report where the parser actually stopped

The ecosystem parity ledger's largest single block was
[#7988](https://github.com/tokuhirom/mutsu/issues/7988): 99 distributions, 77 of
them unable to `use` their own modules, clustered together under the family
`parse-error-expectation-dump` because mutsu described every one of their parse
failures the same useless way — by dumping the set of things its parser would
have accepted, at a location that was almost always the first line of the
module. The ticket called that "99 unknown parse gaps that the ledger cannot
tell apart" and asked for a method, not a fix.

The method turned out to be one line of position arithmetic.

## The location was thrown away at every nesting level

`stmt_list_with_mode` wraps a failed `statement()` into its own
`"expected statement at line N (after M stmts): …"` error, and anchored that
error at `r.len()` — the unconsumed tail at the *start of the statement it had
been trying to read*. A block body is itself a statement list, so the wrapping
happens once per nesting level, and each level overwrote the position with its
own start. The outermost one won.

Concretely: a failure on line 160 of `PrettyDump.rakumod` was reported as

```
expected statement: expected expected statement: expected expected statement: …
at lib/PrettyDump.rakumod:1
------>class PrettyDump {
```

— the `class` opener on line 1, with an expectation set that varies with parser
state rather than with the cause. Nothing in that message names a construct, and
nothing distinguishes it from the other 98 records.

`PError::remaining_len` counts the *unconsumed* tail, so the furthest position
any alternative reached is the **smallest** one. The statement-list loop now
keeps `min(inner, own_start)` instead of discarding the inner failure's
position. The rest of the parser already worked this way —
`update_best_error` has always scored alternatives by how far they got — so this
only stops one site from throwing that score away.

The same failure now reads:

```
at lib/PrettyDump.rakumod:160
------>        -> :(PrettyDump $pretty, $ds, Int:D :$depth = 0 --> Str) {
```

which names a construct: a pointy block taking an explicit signature literal.

## What that turned the cluster into

78 of the cluster's distributions were checked out and re-probed with
`mutsu -I… -e 'use <Module>'`. Before the change, 69 of them failed with a
location that pointed at a `class`/`sub`/`role` opener; after it, every parse
failure names the line the parser actually stopped on. Grouped by *construct*
rather than by message, the ~55 parse failures collapse to roughly forty
distinct gaps, several of which are shared through a dependency:
`Math::FractionalPart`'s `die "…":` reaches three distributions,
`PrettyDump`'s `-> :(…)` three, `Protocol::Postgres`'s `::Enum` type capture
two, `Net::HTTP`'s `multi method CALL-ME(…)` two, `TOML::NQP`'s leading `&&(`
two. So #7988 is a queue of small tickets, not a handful of large ones — which
is the question the ticket asked and could not answer before.

## One of those gaps, fixed with it

`Array::Agnostic`'s `method shape(::?ROLE:D:) { (*,) }` was the first construct
the new location named, and it exposed a real parser bug with nothing to do
with roles.

A punctuation-only name in parentheses followed by whitespace was consumed
*unconditionally* as a call to `prefix:<(…)>`, and the rest of the expression
demanded as its operand. No declaration was required — the spelling alone was
taken as proof that such an operator exists. That shadowed Raku's own `(*)`, a
parenthesised `Whatever`, everywhere a space followed it. The surviving
spellings explain why this went unnoticed for so long: `(*)` at end of input
worked (no whitespace), `(* )` and `( *)` worked (the spelling no longer
matched), and `(*) 1` worked (the "operand" was there) — only `(*)` as a block's
final statement, or `(*,)`, actually broke.

The site now asks `is_user_declared_prefix_sub` first, exactly as the
generic user-operator path immediately below it already does; a genuinely
declared `prefix:<(+-)>` still applies. Three of the sampled distributions
(`Array::Agnostic`, `List::Agnostic`, `MoarVM::Bytecode`) go from
`blocked_load` to loading cleanly on this alone.

Pinned by `t/exceptions/parse-error-position-is-innermost.t` (every asserted
line is the line rakudo reports for the same source) and
`t/lang/operators/paren-prefix-op-requires-declaration.t` (passes under rakudo
too).
