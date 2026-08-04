# An operator declared in a callable block changes how later code parses

```raku
sub run(&c) { c() }
run { sub infix:["@"] ($a, $b) { 42 } }
say EVAL 'sub circumfix:["@", "@"] ($a) { $a }; @ 5 @';
```

rakudo prints `5`. mutsu dies with `Confused. Two terms in a row`, because the
`infix:<@>` declared inside the block is still visible when the `EVAL` string is
*parsed*, so `@ 5 @` reads as an infix application with a missing right operand
instead of the circumfix the string just declared.

## Mechanism

Two known facts compose:

1. mutsu's routine registry is keyed by package alone, so a routine declared in
   a block that runs as a *callable* stays in `registry().functions` after the
   block returns. (`news/2026-08/sibling-scope-routine-shadow.md` fixed the
   redeclaration half of this; the entry itself still outlives its scope.)
2. `Interpreter::collect_operator_sub_names` (`src/runtime/system_eval_string.rs`)
   builds the EVAL parser's operator pre-seed by walking **the whole registry**.

So a stale operator entry does not merely stay callable — it changes the grammar
the next `EVAL` is parsed with. Ordinary nested blocks are fine: `{ sub
infix:["@"] … }` at statement level does not leak, because that path clears the
registry entry. Only the callable-block form does.

## Two possible fixes

- **Narrow.** Pre-seed only operators that are lexically visible at the EVAL
  site. A routine declared inside a callable block leaves a registry entry but
  no `&name` in any visible env tier (this is exactly the asymmetry
  `news/2026-08/eval-sub-shadows-a-registered-routine.md` documents), so
  intersecting the registry walk with `env.visible_keys_where(|k|
  k.starts_with('&'))` would drop the stale ones. Needs checking that a
  compunit-level `sub infix:<…>` really does leave an `&infix:<…>` binding, and
  that operators imported by `use` (which the same function collects from a
  third source) are unaffected.
- **Root.** Give routine declarations real lexical scope, so a block-local
  routine is removed from the registry when its scope ends. That is the same
  change `todo/deep/` wants for the routine-visibility half — a routine declared
  in a sub body is still callable after the sub returns, which rakudo rejects at
  compile time.

## Where it bites

`roast/S06-operator-overloading/sub.t` aborts on it after 24 of 29 assertions:
the file declares `sub infix:["@"]` inside a `lives-ok { … }` and then EVALs a
string declaring `circumfix:["@", "@"]`. See the ledger entry in
`todo/tickets/vendor-real-test-module.md` for the file's other two blockers.
