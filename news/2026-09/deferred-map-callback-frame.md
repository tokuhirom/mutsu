# A deferred `.map` callback resolves its free variables lexically again

A deferred `.map` callback (ADR-0058) runs when the `Seq` is *consumed*, which
is routinely a different frame than the one that called `.map`. Its free
variables read the wrong binding there: a same-named lexical that happened to be
live in the **consuming** frame won over the one captured at the `.map` call
site.

The shape it was found through is a recursive producer, where the consuming
frame is an *outer invocation of the very routine that created the callback*:

```raku
my $d = 0;
sub g(@sizes) {
    $d++; die "DEEP" if $d > 8;
    note "d=$d sizes=" ~ @sizes.raku;
    return "STOP" if @sizes == 0;
    [1].map(-> $e { g(@sizes[1..*]).map(-> $x { $x }) })
}
say g((2,1)).raku;
```

| | before | after (= rakudo) |
|---|---|---|
| depths reached | 1, 2, 3, 4, 5, … (runaway) | 1, 2, 3 |
| `@sizes` at depth 3 | `(1,)` — depth 1's, one subscript on | `()` |
| result | `Seq.new()` | `(($(("STOP",).Seq),).Seq,).Seq` |

`g` terminates on `@sizes == 0`. It never got there: at depth 3 the callback
read `@sizes` from the frame active at the *pull*, so `@sizes[1..*]` was
perpetually `(1,)`. The parameter was bound correctly at every call — the `note`
proved it — so this was a read landing in the wrong frame, i.e. silent data
loss with a stack-overflow face. It aborted the whitelisted
`roast/integration/99problems-21-to-30.t` when ADR-0058 step 3 landed, which is
why step 3 was reverted (ADR-0058 §9).

## Root cause: a merge priority, not a missing frame

The ticket, and ADR-0058 §9 with it, recorded the cause as `SeqSource::MapGrep`
carrying no env snapshot where the older `create_lazy_map_list` deferral carries
one, and listed three fixes that all revolved around *adding* a snapshot. That
diagnosis was wrong in a useful way: the callback's own closure env **does**
carry the right `@sizes`. It was being discarded.

`eval_map_over_items` merges the callback's captured env into the running frame
with **caller priority**, with two exceptions: `self` (lexical), and a captured
`ContainerRef`, which is a shared container cell and therefore the single source
of truth for that lexical (ADR-0025/ADR-0055 §1.2(b)). The cell exception
covered only the lexicals `box_captured_lexicals` actually boxes — in practice
`$`-scalars. An `@`/`%` container free variable is captured *by value*, matched
neither exception, and so lost to whatever the consuming frame held under the
same name.

That is why the divergence looked frame-shaped: it is only observable when the
pull happens in a different frame than the `.map` call, which is exactly what
ADR-0058 made the normal case.

The fix is one rule in that merge: a captured value for one of the block's own
**free variables** wins, for the same reason the cell does — a free variable is
lexical by definition, so it names the binding at the block's creation site,
never a same-named lexical live in the consumer. Dynamic variables (`$*x`) keep
caller priority, being dynamic-scope by design. `CompiledCode::capture_free_var_set`
already exists and is built once per chunk, so the rule costs a set lookup per
captured key and adds no allocation, no env clone, and nothing to
`env_deep_copies` — which is what the three snapshot-shaped options were being
measured against.

## Measured

`$`-scalars were already right; `@` and `%` were not. All four now agree with
rakudo:

| probe | before | rakudo / after |
|---|---|---|
| `sub mk(@p) { [1].map({ @p.elems }) }`, consumed where `my @p = 1` | `(1,)` | `(3,)` |
| … consumed by a routine whose own **parameter** is `@p` | `(1,)` | `(3,)` |
| a file-scope `my @sizes` shadowed by a consumer lexical | `(1,)` | `(3,)` |
| the `%`-container spelling | `('inner',)` | `('outer',)` |
| the `$`-scalar spelling | `('outer',)` | `('outer',)` |

Controls that must not move, and do not: a lexical mutated *after* closure
creation, and one mutated after the `.map` call but before the pull, are both
still seen at the pull (they travel through the shared cell).

Pinned by `t/deferred-map-callback-frame.t` (9 tests, green under rakudo too).

## What this unblocks

ADR-0058 **step 3** (deferring the listop `map` form) and **step 4** (retiring
`create_lazy_map_list`, the only deferral that got the frame right) were parked
behind this and are now unblocked. `builtins_collection_mapgrep.rs`'s step-3
diff is unchanged from the parked attempt; ADR-0058 §5 still requires a full
`make roast` before it lands.
