# `.^find_method(name).candidates` misses an inherited non-multi method

`Interpreter::classhow_lookup_all_candidates` (`src/runtime/methods_classhow_lookup.rs`,
the function immediately after `classhow_lookup`, used by `.^find_method(name).candidates`)
has the same "receiver's own class only" bug that `classhow_lookup` had before ADR-0019 E7
step 5 fixed it (`todo/deep/adr0019-e5-e7-entry-routing.md` "E7 step 5"), but only for the
**non-multi** branch:

```rust
let owners: Vec<String> = if class_method_is_multi(class_name) {
    // ... correctly computes and reverses the full MRO, filtering to
    // levels whose own `method_name` is multi ...
} else {
    vec![class_name.to_string()]   // <-- only the receiver's own class, no MRO walk
};
```

When `method_name` is a plain (non-multi) method that only the receiver's own class defines,
this is correct. But when it is declared only on an ancestor, `owners` never includes that
ancestor, so `.^find_method(name).candidates` returns an empty list instead of the one
candidate real Raku reports.

**Confirmed with `raku`** (`tmp/lookup-candidates.raku` style repro):

```raku
class A { method foo { "A::foo" } }
class B is A {}
say B.^find_method("foo").candidates.elems;   # raku: 1
```

mutsu currently answers `0` for this (unverified against the exact current build at time of
filing — verify before fixing, since E7 step 5 landed on the same day and touched the
sibling `classhow_lookup` function in the same file; `classhow_lookup_all_candidates` itself
was NOT touched by that PR).

**Why this was not folded into E7 step 5's fix**: it is a different function with a
different candidate-list shape (multi-vs-non-multi owners, not "first def wins"), so the fix
is not a literal one-line change of the same shape — it needs its own MRO walk that finds
the first (most-derived) owning class for the non-multi case, mirroring what E7 step 5 did
for `classhow_lookup`. Per the ADR-0019 Phase E "one consumer family per sub-PR" discipline,
this is left as a follow-up rather than expanding that PR's scope.

**Suggested fix shape**: in the `else` branch, walk `self.class_mro(class_name)`
(most-derived first) and take the first level whose own `class_def.methods` contains
`method_name`, using that level's class name as the sole owner (`vec![owner]`) instead of
always `vec![class_name.to_string()]`. This mirrors the MRO walk `classhow_lookup` now does
for its own non-multi tier.
