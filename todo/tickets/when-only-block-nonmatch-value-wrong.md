# A block whose only statement is a non-matching `when`/`default` evaluates to the wrong value

## Discovered while fixing

`todo/tickets/map-grep-inline-block-swallows-succeed.md` (now
`news/2026-08/map-grep-inline-block-succeed-signal.md`) fixed `.map`/`.grep`
losing an item entirely when an inline block's `when`/`default` raises a
succeed signal on a MATCH. While verifying that fix against inputs that
also contain non-matching items, a separate, pre-existing bug surfaced: the
value an inline map/grep block produces for an item that matches NO
`when`/`default` branch is wrong.

## Repro

```
$ mutsu -e 'my @a = (1,"a",2).map({ when Int { "int" } }); say @a.raku'
["int", "a", "int"]
$ raku  -e 'my @a = (1,"a",2).map({ when Int { "int" } }); say @a.raku'
["int", 0, "int"]
```

```
$ mutsu -e 'my @a = (1,"a",2,"b").grep({ when Int { True } }); say @a.join(",")'
1,a,2,b
$ raku  -e 'my @a = (1,"a",2,"b").grep({ when Int { True } }); say @a.join(",")'
1,2
```

mutsu's non-matching item falls back to the ORIGINAL topic value (`"a"`
stays `"a"` in the map case; `"a"`/`"b"` are treated as truthy in the grep
case, so nothing is filtered at all). raku's non-matching item evaluates
to something else entirely (`0` in the map probe — not obviously `Nil`,
worth re-checking against `raku-doc/doc/Language/control.rakudoc`'s
`when`/`given` semantics before assuming what the "right" value is; a
third probe, a raw non-block-call `{ when 2 { True } }` invoked directly
per-item, printed `False` for non-matching items, suggesting a boolean
coercion is involved somewhere in the real semantics, not a bare `Nil`
passthrough).

## Root cause (partial — needs more investigation)

In the map/grep inline fast-path `Ok(())` arms (`eval_map_over_items` in
`src/runtime/resolution_map_grep.rs`, `eval_grep_over_items_with_mutated`
and `eval_map_over_items_rw` in `src/runtime/resolution_map_grep_rw.rs`),
the block's result value is computed as:

```rust
vm.last_stack_value().cloned().or_else(|| vm.env().get("_").cloned()).unwrap_or(Value::NIL)
```

When a `when`/`default` chain is the block's only statement and it does
NOT match, nothing is pushed to the stack (`last_stack_value()` is
`None`), so this falls back to `vm.env().get("_")` — the CURRENT TOPIC,
i.e. the original unfiltered item — instead of whatever raku's real
non-match value is. This `.or_else` fallback exists for a different
reason (reading back a `$_`-mutated value for `<->`/rw map semantics, per
the `writeback` closures nearby) and is firing incorrectly here.

This bug was previously unobservable through map/grep + `when`, because
ANY match in the same call would abort the whole map/grep with the (now
separately fixed) "swallows succeed" crash before this fallback path's
wrongness could surface. Fixing that crash made this second, independent
bug newly visible.

## Fix direction

1. Determine raku's actual non-match value for a when-only block in
   value/list context (re-run the three probes above, plus a plain
   `given`/`when` outside any map/grep, and read
   `raku-doc/doc/Language/control.rakudoc`'s `given`/`when` section).
2. Distinguish "the block genuinely produced no value" (falls through a
   non-matching `when` chain) from "the block's last statement is a bare
   `$_`, meant to reflect back a possibly-mutated topic for rw semantics"
   — the current shared `.or_else` fallback conflates the two. This likely
   needs compile-time knowledge (does the block contain a `when`/`default`
   chain as its tail statement?) or a distinct runtime marker separate
   from "nothing on the stack".
3. Apply consistently across all four `Ok(())` arms found by
   `grep -n "or_else(|| vm.env().get(\"_\")" src/runtime/resolution_map_grep*.rs`.

Risk: medium — this fallback is load-bearing for rw map/grep (`<->`,
`.=map`), so the fix must not break that (`t/` has existing rw map/grep
pins — run them after any change).

## Verification

- The two repro one-liners above should match raku's actual (re-verified)
  output.
- Existing rw map/grep pins (`grep -rl "map_over_items_rw\|<->" t/`)
  still pass.
- Add a `t/` pin covering the non-match case for both `.map` and `.grep`
  with a `when`-only block.
