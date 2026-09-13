# The ecosystem ledger's second ticket batch: one message was three bugs

The first P5 batch (2026-09-11) filed fifteen root-caused issues off the parity
ledger and stopped at the ten-distribution line, leaving "a long tail: 985
clusters, a sampling job". This is the second pass, going below that line:
twelve issues covering about 110 further distribution slots, all verified
against the `raku` oracle and minimised before filing.

| issue | dists | root cause |
|---|---|---|
| [#8209](https://github.com/tokuhirom/mutsu/issues/8209) | 19 | a `{`/`}` inside a string literal ends a regex code assertion early |
| [#8210](https://github.com/tokuhirom/mutsu/issues/8210) | 18 | `use Foo:if($cond)` — `Raku.legacy` unimplemented, so the `if` pragma dies in `EXPORT` |
| [#8215](https://github.com/tokuhirom/mutsu/issues/8215) | 13 | missing nqp ops `push`, `hash`, `p6bindattrinvres` |
| [#8211](https://github.com/tokuhirom/mutsu/issues/8211) | 12 | `.grep`/`.first`/`.classify` on a non-`%`-sigil Hash hand the block the Hash, not its Pairs |
| [#8214](https://github.com/tokuhirom/mutsu/issues/8214) | 9 | a local `sub` cannot shadow a routine imported by `sub EXPORT` |
| [#8213](https://github.com/tokuhirom/mutsu/issues/8213) | 8 | `OUTER::MY::` does not resolve, `MY::` sees outer lexicals, `<<$name>>` throws |
| [#8212](https://github.com/tokuhirom/mutsu/issues/8212) | 7 | `role R[::T] does R` — a parameterised role cannot compose its own base |
| [#8216](https://github.com/tokuhirom/mutsu/issues/8216) | 5 | an anonymous role literal as a `does` argument reads as a role named `role` |
| [#8217](https://github.com/tokuhirom/mutsu/issues/8217) | 5 | `use NativeCall::Types` is not resolvable on its own |
| [#8218](https://github.com/tokuhirom/mutsu/issues/8218) | 5 | a `my class` inside a `module` is installed into that package |
| [#8219](https://github.com/tokuhirom/mutsu/issues/8219) | 1 | a `when` whose matcher is a comma list does not parse |
| [#8220](https://github.com/tokuhirom/mutsu/issues/8220) | 1 | `CORE::<&infix:<+>>` mis-parses by context, and `CORE::` holds no operator routines |

## The finding that changes how the tail should be read

`scripts/ecosystem-tickets.py` clusters by normalised error message, and below
the ten-distribution line that stops being a proxy for a root cause. The clearest
case is `eco-cluster: 5ed688f7`: 26 distributions, every one reporting
`X::Syntax::Missing: Missing block`, which reads like one parser bug. Bisecting
its members' files gave **three unrelated bugs** — the brace-in-a-string-literal
assertion extent (#8209), a comma-list `when` (#8219), and the `CORE::<&op>` pair
(#8220) — plus one member already recorded in #7954's table.

The lesson is that a *named* exception is no more of a root cause than the
expectation dump #7988 is about. `X::Syntax::Missing` names the parser's
recovery point, not the construct; only the located line does. The existing
"verify a cluster before filing it" rule therefore applies to the named
`X::Syntax::*` buckets exactly as it does to `expected statement …`, and the way
to work one is to bisect the file rather than to read the message.

Cluster count misleads in the other direction too. The three `nqp-op` clusters
(`push`, `hash`, `p6bindattrinvres`) are distinct signatures with a single fix
site, so they were filed as one ticket the way #8024 was.

## Leverage still concentrates in dependencies

#8209's 19 distributions are the whole `DSL::*` family, and every one of them
fails on **the same line of the same dependency** —
`DSL::Shared::Roles::CommonStructures.rakumod:127`, a `regex` whose code
assertion contains `'${'`. #8210's list contains `Cro::HTTP` and
`Cro::WebSocket`, so its real reach is wider than the 18 records that name it.
This is the same shape as the first batch's #7999 (one parse gap in
`Terminal::Widgets` blocking 16 distributions): a ticket's value is set by what
depends on the distribution it was found in, not by its own record count.

## Two clusters examined and deliberately not filed

- `eco-cluster: 7e654040` (`Type Array does not support associative indexing.`,
  8 dists) is heterogeneous: `OrderedHash` reaches it through `my %h does
  OrderedHash[…]`, while `Ujumla` and `Docker::File` reach it from test files
  with no such declaration. One ticket over those would be a mega-ticket no
  single fix closes.
- `eco-cluster: 4aad304a` (`Function 'X' needs parens to avoid gobbling block`,
  6 dists) did not reproduce from the construct its reported line names — a
  qualified typename in a `when` works today — so those lines are containers,
  not causes.

`eco-cluster: f15ddd40` (`check-phaser`, 21 dists) is waiting on a re-measure
rather than on triage: #8000 made the `CHECK` message carry its inner exception,
and it landed after the sweep these records came from.

Filing nothing for a cluster you could not reduce is the right outcome; the cost
of a vague ticket is paid by whoever picks it up.
