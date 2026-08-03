# A tied multi candidate should be broken by declaration order

When several multi candidates are equally narrow, Raku picks the one declared
first. mutsu picks the first of the *sorted candidate list*, and
`sort_candidates_by_specificity` (`src/runtime/dispatch_resolve.rs`) breaks an
equal-rank tie on the **registry key string**:

```rust
candidates.sort_by(|a, b| {
    let a_rank = self.candidate_specificity_rank(&a.1);
    let b_rank = self.candidate_specificity_rank(&b.1);
    b_rank.cmp(&a_rank).then(a.0.cmp(&b.0))   // <- key string, not declaration order
});
```

The candidates themselves are collected by iterating `registry().functions`, a
hash map, so nothing upstream preserves declaration order either.

Now that named parameters no longer contribute to narrowness
(`news/2026-08/named-params-do-not-narrow.md`), ties are more common and the gap
is visible:

    proto h(:$a) {*}
    multi h(    :$a) { "untyped" }
    multi h(Str :$a) { "typed" }
    say h(a => "x");        # rakudo: untyped    mutsu: typed

    proto h3(:$a) {*}
    multi h3(    :$a) { "untyped" }
    multi h3(Any :$a) { "any" }
    say h3(a => "x");       # rakudo: untyped    mutsu: any

Both candidates are applicable and equally narrow, so declaration order should
decide; mutsu instead lands on whichever key sorts later. (The `Any :$a` vs
`Int :$a` pair *does* come out right today, so this is not uniformly wrong —
which is exactly why it needs a real ordering key rather than more tuning.)

## The fix

`FunctionDef` already carries `decl_order: u64` (`src/ast.rs`), a monotonic
registration stamp — but it is only set for `token`/`rule` proto candidates, by
`insert_token_def`, and is 0 for everything else. Stamp it for every routine
registration and use it as the final tie-break in
`sort_candidates_by_specificity` (and in `choose_best_matching_candidate`'s
`ranked.sort_by`, whose stability then carries it through).

The work is small but touches every registration path (`registration_sub.rs`,
the class/role method paths, `EXPORTHOW`/`EVAL` re-registration), and the stamp
has to survive precompilation — `decl_order` is `#[serde(default)]`, so a
precompiled unit that predates the change deserializes every candidate as 0 and
silently loses the ordering. Getting that right, and deciding whether the stamp
is global or per-`(package, name)`, is what makes this a ticket rather than a
one-line patch.
