# Subscript-adverb Pairs (`:p`/`:kv`) now carry a live element container (ADR-0036 slices 1-2)

Raku's `@a[0]:p` is `0 => @a[0]` where the *value is the element's `Scalar` container* — writing
`.value = X` writes the array, and reading `.value` later sees subsequent writes to the array. mutsu
used to build that Pair from a clone of the element and then compensate for the missing link with a
runtime search of `self.env` at assignment time (`assign_method_lvalue_with_values`), looking for an
array/hash whose element happened to compare equal to the pair's snapshot value. That search produced
twelve measured divergences from raku (design pass: [`docs/adr/0036-element-container-pairs-from-subscripts-and-pairs.md`](../../docs/adr/0036-element-container-pairs-from-subscripts-and-pairs.md)
§1.3): stale reads through the pair (no search can ever fix these — they need a live cell), and
ambiguity failures where an ordinary `my @b = @a;` anywhere in scope gave the search a second
equal-valued candidate, making `:p` writes die with a misleading `X::Assignment::RO ... on
non-instance` and `.pairs` writes silently do nothing.

Slices 1-2 of the ADR landed:

- **Slice 1** added `t/subscript-pair-element-container.t`, pinning all twelve §1.3 divergence rows
  (plus the `.VAR.^name` container-identity probes) as the acceptance oracle for the whole campaign,
  `todo`-marking the rows that need later slices.
- **Slice 2** routed `:p` and `:kv` in `builtin_subscript_adverb`
  (`src/runtime/builtins_multidim_subscript.rs`) through the existing `Value::array_slot_ref` /
  `Value::hash_slot_ref` primitives — the same ones that already give `my $r := @a[0]` write-through,
  read-through, and no-ambiguity semantics — whenever the source is a genuine mutable Array or Hash
  (not a List/Range/Seq coercion, not a QuantHash `.hash` projection, not an AT-KEY-instance snapshot).
  This is routing over a primitive that already shipped for the `:=` bind path, not new machinery. The
  `:kv` parser rewrite that used to make `(@a[0]:kv)[1] = x` work "by accident" for one syntactic shape
  only (`src/parser/stmt/assign/lvalue.rs`, `src/parser/expr/precedence/logic.rs`) was deleted in the
  same slice — the construct now reaches the same outcome through the ordinary index-assign
  write-through-a-`ContainerRef` path instead.

Six of the twelve §1.3 rows are green (rows 1, 2, 5, 6, 7, 8 — a correction from the ADR's original
estimate of seven: row 12's typed-array element constraint needs slice 4's `register_container_constraint`
work, which slice 2 does not touch). Rows 3, 4, 9, 10 and 11 need slice 3 (`.pairs`/`.kv`/`.antipairs`
at the VM method dispatch layer); row 12 needs slice 4. Both remain open and are tracked in the ADR's
"Implementation status" section, not as a separate `todo/` finding — this entry retires
`todo/deep/subscript-p-pair-is-a-snapshot-not-a-container.md`, whose own status note named slice 2's
landing as exactly the point to fold it in here.

One correctness subtlety surfaced during implementation: the array branch's `:delete` companion
overwrites the live slot with a hole *before* formatting the adverb's rows, so promoting to a container
*after* delete would have hand back a cell around the fresh hole instead of the pre-delete value the
adverb must report (`roast/S09-subscript/slice.t`'s "Nested slice, delete + p/kv adverbs" subtests
caught this). Fixed by skipping the container-aware path whenever a `:delete` companion is present,
falling back to the plain snapshot value there — matching raku, where a deleted slot has nothing left
to alias.
