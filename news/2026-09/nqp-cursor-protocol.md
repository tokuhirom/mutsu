# The NQP cursor protocol is available to user code

Rakudo's regex engine drives a regex by hand through a *cursor* — a `Match` that has not finished
matching yet — and exposes that protocol to user code. Ecosystem distributions reach for it to match
at a known position and read the resulting cursor's `$!from` / `$!pos` instead of going through `~~`
and a `Match` object. mutsu had none of it: `Match.^lookup("!cursor_init")` answered `Nil`, so the
whole idiom died at its first line ([#7883](https://github.com/tokuhirom/mutsu/issues/7883)).

```raku
my $cursor-init = Match.^lookup("!cursor_init");
my $cursor := /foo/($cursor-init(Match, "xfoox", :0c));
say $cursor.pos;   # 4
```

## What the issue expected to be needed, and what actually was

The issue priced this as three deep problems: private methods in `Match`'s MOP surface, a cursor as
a first-class value distinct from a `Match`, and a new calling convention for `Regex`. Measuring
rakudo 2026.07 shrank all three:

- `!cursor_init` is not a Raku private method. A leading `!` is an ordinary identifier character in
  NQP, and rakudo lists `!cursor_init` among `Match.^methods` and answers `Match.^can("!cursor_init")
  .elems` with 1 — so it needed a named source in `.^lookup`, not the `is_private` machinery.
- A cursor **is** a `Match`: `$cursor-init(Match, "xfoox", :0c).^name` is `Match`, not some `Cursor`
  type. mutsu's own `Match` representation carries it, with rakudo's `$!orig` / `$!from` / `$!pos`
  mapped onto `orig` / `from` / `to` (`.pos` already read `to`).
- The calling convention already had a precedent: `regex_token_method.rs` has run a grammar token
  method against a `Match` cursor since the custom-HOW `find_method` work.

The one genuinely new thing measurement turned up is the scan/anchor discriminator. `$!from == -1`
is rakudo's own "this cursor has not started matching" marker, and it — not the regex, not the call
site — is what decides whether a cursor call scans forward or anchors:

| init | `$!from` | `$!pos` | `/foo/` on `"xfoox"` |
| --- | --- | --- | --- |
| `:c(0)` | -1 | 0 | from=1 pos=4 (scanned) |
| `:p(0)` | 0 | 0 | from=0 pos=-3 (anchored, failed) |
| `:p(1)` | 1 | 1 | from=1 pos=4 (anchored, matched) |

A failed cursor is a *failed* `Match` — `.defined` is `True`, `.Bool` is `False`, it gists as
`#<failed match>` — whose `$!from` stays at the position the attempt started from and whose `$!pos`
is -3, NQP's backtracking-state marker.

## What shipped

`src/runtime/regex/regex_cursor.rs` owns the protocol: which methods `Match` answers for, how a
cursor is built from `:c` / `:p`, and the shared "match at (or from) this cursor and wrap the
outcome" body. `.^lookup` / `.^find_method` / `.^can` consult it as their own named source, because
`Match`'s builtin-catalog row carries no methods at all. `Regex.CALL-ME` on a cursor and the
`!cursor_init` re-dispatch both funnel through `call_method_with_values`, gated on the method name.
The existing token-method path gained the scanning half for a not-yet-started cursor.

The issue's claim that `nqp::getattr_i` "itself works now" was not true when this was filed --
there was no `getattr` op at all, only `bindattr`. A sibling `String::Utils` slice landed the
`nqp::getattr` family (and `nqp::substr`, `nqp::index`, ...) on `main` while this was in flight, so
what remains here is the `Match` half of it: a `Match`'s NQP-level attribute names are not the keys
mutsu stores, and a still-lazy `Match` is not an `Instance` at all, so neither the bare-name nor the
twigil spelling reached them. `nqp_attr_value` now aliases `$!pos` / `$!from` / `$!orig` onto
`to` / `from` / `orig`, which is what makes `nqp::getattr_i($cursor, Match, '$!pos')` answer.

## The consumer

`String::Utils`'s `replace` (`lib/String/Utils.rakumod:563`) is written entirely in this idiom, so
every `replace`/`replace-all` assertion in that distribution's `t/01-basic.rakutest` *died* rather
than failing. Lifted verbatim into `t/regex/match/match-cursor-protocol.t`, it now produces rakudo's
answers.

## Scope

Only the entry point the idiom needs. The rest of the protocol (`!cursor_start`, `!cursor_pass`,
`!cursor_capture`, the `$!shared` / `$!braid` state NQP threads through a parse) stays internal to
mutsu's engine: a cursor here is produced complete, never advanced step by step by user code. One
consequence is that a cursor mutsu returns carries its captures, where rakudo's carries none until
`!reduce` builds the Match — strictly more information, and not something the idiom reads.

The pin is `t/regex/match/match-cursor-protocol.t`: 32 assertions, every expectation measured
against rakudo 2026.07, and the file passes unchanged under both interpreters.
