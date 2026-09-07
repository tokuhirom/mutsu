# A deferred `grep`'s capture merge stops clobbering the consuming frame

ADR-0058 step 3b made `.grep` produce a deferred `Seq`, so its callback now runs
at the *pull*, in whatever frame consumes the Seq. The grep loop's captured-env
merge was still written for the old timing, and it had a shape `.map`'s
equivalent had already been fixed out of:

```rust
// what was saved for restoration
for k in data.env.keys() {
    if !self.env.contains_key_sym(*k) { touched_keys.push(k.resolve()); }
}
// ... what was actually overwritten
for (k, v) in &data.env {
    self.env.insert_sym(*k, v.clone());          // every captured name
}
```

Every captured name was overwritten; only the names the consuming frame did
*not* already hold were saved. A name held by both was therefore replaced with
its capture-time value and never put back.

While `.grep` ran eagerly at the call site this was invisible: the captured env
was the same frame's env, an instant earlier. Deferred, the minimal victim is
the variable the Seq is being assigned to, because it is captured mid-statement
while its own right-hand side is still evaluating:

```raku
my $s = (1, 2, 3, 4).grep({ $_ %% 2 });   # `s` is captured here, as Any
say $s.elems;                             # 2   -- the pull runs here
say $s.^name;                             # mutsu: Any     rakudo: Seq
```

The env write is only authoritative once `reflective_name_access_possible()`
has latched (before that the local slot is the single source of truth and the
env clobber is inert), so in practice it took any program that `EVAL`s — which
is every file that loads the real upstream `Test` module, whose `throws-like`
EVALs a string. That is how it surfaced: `t/seq-array-context-reiterate.t`'s
`is @$s.sum, 6` passed once and then read `0`.

`.first`'s batched matcher (`try_first_match_batched`) carried the identical
shape and is fixed with it.

Both now use the same caller-priority merge `eval_map_over_items` does, through
the same `capture_wins_over_caller` predicate: a captured `ContainerRef` cell
and a captured value for one of the block's own free variables are lexical and
win; everything else keeps caller priority; and every name the merge overwrites
is saved for restoration. The two halves of the same operation agree about the
env again, which was the point of making them agree about timing.

Pinned by `t/grep-capture-merge-keeps-caller-lexicals.t` (8 subtests, identical
output under `raku`).
