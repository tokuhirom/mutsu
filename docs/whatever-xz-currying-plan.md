# Plan: X/Z meta-operator Whatever-currying (whatever.t test 77+)

Handoff for the next session. Goal: make `S02-types/whatever.t` get past its abort
at **test 77** (`my $f = (1 X+ * X+ 3); isa-ok $f, Code`) by giving X/Z
meta-operators Whatever-currying, **without regressing the whitelisted
`S03-metaops/zip.t` and `cross.t`**.

## Status / scope

- `whatever.t` currently runs **77/131** then aborts at test 77 (`1 X+ * X+ 3`
  evaluates to a `List`, then `$f(2)` crashes "No such method CALL-ME for List").
- The abort blocks tests 78–131 (~54 tests). Clearing it reveals how many pass.
- This is one of "33 distinct WhateverCode features" in this Hard file; this plan
  covers only the X/Z meta-op currying cluster (tests 77–92-ish).

## Expected behavior (verified against `raku`)

CURRY — a *standalone* Whatever operand of X/Z makes a WhateverCode:
```raku
(1 X+ *)(2)      # 6          ($f.^name eq 'WhateverCode')
(* X+ *)(41,43)  # 84
(1 Z+ *)(2)      # 6
(1 X.. *).^name  # WhateverCode  (range op forms curry too)
(* Z 10).^name   # WhateverCode  (bare * standalone is a curry)
```

EXTEND — a Whatever as the *trailing element of a comma-list operand* repeats the
last element; it is NOT a curry (this is the zip.t/cross.t behavior to preserve):
```raku
(1, 2, 3, * Z 10, 20, 30)        # (1,10, 2,20, 3,30, 3,40, 3,50) — repeats the 3
(1, 2, 3, * Z+ 10, 20, 30, 40, 50) # (11, 22, 33, 43, 53)
```

So the rule: **a bare/compound `*` that is a complete X/Z operand curries; a `*`
that is the last element of a list operand extends.**

## Where the machinery lives

Whatever-currying is a PARSE-TIME transform (`src/parser/expr/mod.rs`):
- `contains_whatever(expr)` (~line 674) decides whether to wrap an expr in a
  WhateverCode. The `Expr::MetaOp` arm is currently gated to **`meta == "R"`**
  only — X/Z are deliberately excluded (the original comment cited the "extend"
  case).
- `count_whatever(expr)` (~line 777) — same `meta == "R"` gate; counts `*`s to
  decide the WhateverCode's arity.
- `wrap_whatevercode` / `replace_whatever_numbered` (~944) / `replace_whatever_single`
  (~1233) / `expr_contains_topic` (~1129) — these ALREADY handle `Expr::MetaOp`
  for all metas (not R-gated). So the wrap/replace side needs no change.

## Step 1 — the scalar cases (works, but incomplete on its own)

Enable X/Z in `contains_whatever` and `count_whatever`. A safe form that also
respects the extend rule at the MetaOp-node level:

```rust
// contains_whatever, replacing the `meta == "R"` arm:
Expr::MetaOp { meta, left, right, .. } if meta == "R" => {
    contains_whatever(left) || contains_whatever(right)
        || is_wrapped_whatevercode(left) || is_wrapped_whatevercode(right)
}
Expr::MetaOp { meta, left, right, .. } if matches!(meta.as_str(), "X" | "Z") => {
    fn operand_curries(e: &Expr) -> bool {
        // a list operand's trailing `*` is an extender, not a curry placeholder
        !matches!(e, Expr::ArrayLiteral(_))
            && (contains_whatever(e) || is_wrapped_whatevercode(e))
    }
    operand_curries(left) || operand_curries(right)
}
```
(and the analogous `matches!(meta.as_str(), "R" | "X" | "Z")` in `count_whatever`.)

This makes the **scalar** cases (`1 X+ *`, `* X+ *`, `1 Z+ *`) curry → whatever.t
77–82 pass, abort moves 77 → 83. BUT it REGRESSES zip.t/cross.t — see Step 2.

## Step 2 — the real blocker: comma-list operand lifting (`container.rs`)

The X/Z **left** comma-list operand is gathered by a post-parse "lift", NOT by
`sequence_expr`. Trace:
- `1,2,3 Z 10,20,30` is first parsed as the list `[1, 2, MetaOp{Z, 3, [10,20,30]}]`
  (the Z binds `3` as its left, gathers `10,20,30` as the right list).
- `finalize_paren_list` → **`lift_meta_ops_in_paren_list`**
  (`src/parser/primary/container.rs:517`) finds the trailing `Expr::MetaOp`,
  takes the preceding items `[1,2]` + the metaop's own left `3` as the first
  column, and rebuilds `MetaOp{Z, [1,2,3], [10,20,30]}`. The runtime Z handler
  then does cross/zip (and extend on a trailing `*`).

**The bug:** with Step 1, `1,2,3,* Z 10,20,30` wraps `* Z 10,20,30` into a
`Lambda{is_whatever_code, body:[Stmt::Expr(MetaOp{Z,*,[10,20,30]})]}` during
parsing. `lift_meta_ops_in_paren_list` looks for `Expr::MetaOp` and does not see
through the `Lambda`, so the list is NOT lifted — it stays
`ArrayLiteral([1,2,3, Lambda{...}])`. Result: a List with a stray WhateverCode
instead of the extend. zip.t tests 14/15/17/18 fail.

**The fix (do this for a clean, non-regressing change):** teach
`lift_meta_ops_in_paren_list` (and the `position(...)` at line 518) to also match
a trailing **whatever-wrapped MetaOp Lambda**:
1. When the trailing item is `Lambda{is_whatever_code:true, body:[Stmt::Expr(MetaOp{X|Z,..})]}`
   (or the `AnonSubParams` multi-param form), unwrap it to recover the inner
   `MetaOp`. (Note the inner operand `*` was already replaced with `_`/`__wc_N`
   by `replace_whatever_*`; you may need to lift BEFORE wrapping instead — see
   "Alternative" below.)
2. Lift the comma-list operands into the MetaOp exactly as today, producing
   `MetaOp{Z, [1,2,3,<*>], [10,20,30]}`.
3. Re-apply the curry-vs-extend decision on the lifted MetaOp using the
   `operand_curries` rule (list operand with trailing `*` → **extend**, leave a
   plain MetaOp; standalone `*` → **curry**, re-wrap via `wrap_whatevercode`).

**Alternative (cleaner):** suppress the WhateverCode wrapping for an X/Z MetaOp
*while it is still a comma-list element*, let `lift_meta_ops_in_paren_list` rebuild
the full `MetaOp{Z, [..], [..]}`, and only THEN run `contains_whatever` /
`wrap_whatevercode` on the lifted MetaOp. At that point the operands are proper
`ArrayLiteral`s, so `operand_curries` correctly distinguishes
`(1,2,3,*) Z (10,20,30)` (extend) from `(1,2) X~ * X~ (3,4)` (curry). This avoids
the unwrap-the-Lambda dance entirely — the lifting must happen before the curry
decision.

## Known harder sub-case (can defer)

`(1,2 X~ * X~ 3,4)` (whatever.t test 83/84) — `*` is a STANDALONE middle operand
between two `X~`, with list operands `(1,2)` and `(3,4)`. After lifting it should
curry to a 1-arg WhateverCode whose body is `(1,2) X~ _ X~ (3,4)`. The
`$f(<a b>)` call must substitute `<a b>` for the `*`. If the Step-2 lift runs
before the curry decision (the Alternative), this should fall out; verify.

## Verify

```bash
cargo build
# scalar curry
target/debug/mutsu -e 'my $f=(1 X+ * X+ 3); say $f.^name; say $f(2)'   # WhateverCode, (6)
target/debug/mutsu -e 'my $f=(* X+ *); say $f(41,43)'                  # (84)
# extend MUST still work (zip.t)
target/debug/mutsu -e 'say (1,2,3,* Z 10,20,30)'  # (1 10 2 20 3 30 3 40 3 50)
# regression gates (both whitelisted — must stay 0):
MUTSU_FUDGE=1 prove -e target/debug/mutsu roast/S03-metaops/zip.t roast/S03-metaops/cross.t
# progress:
MUTSU_FUDGE=1 target/debug/mutsu roast/S02-types/whatever.t 2>&1 | grep -E 'not ok|ran|Planned'
```

## Files

- `src/parser/expr/mod.rs` — `contains_whatever` (~674), `count_whatever` (~777).
- `src/parser/primary/container.rs` — `finalize_paren_list` (309),
  `lift_meta_ops_in_paren_list` (517), `normalize_chained_zip_meta` (412).
- Tests: `roast/S02-types/whatever.t` (target), `roast/S03-metaops/zip.t` +
  `cross.t` (regression gates, whitelisted).

## Other whatever.t failures (separate features, not this cluster)

- Test 42: `* = 5` dummy assign — `# TODO`, also fails on raku.
- Tests 75/76: `&infix:<+>(*, 42)` / `&infix:<R+>(*, 42)` must **die**, not curry.
  mutsu over-curries: the `CallOn` arm of `contains_whatever` (~633) wraps a call
  when an arg is a bare `*`. Raku never curries a function call on its args (`*`
  passed to a sub is a plain Whatever value that dies when used). Risky to change —
  the CallOn currying likely supports other tests; needs its own investigation.
