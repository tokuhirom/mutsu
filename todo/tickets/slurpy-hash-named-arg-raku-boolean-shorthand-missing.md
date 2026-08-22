# `.raku` on a `Hash` populated from slurpy `*%h` named-arg binding doesn't abbreviate Bool::True pairs

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Pair.rakudoc:61`).

## Root cause hypothesis

When a caller passes boolean-valued named arguments (`s :a1:b2;` or `s a => True;`) that get
collected into a callee's slurpy `*%h` parameter, real `raku`'s `Hash.raku` renders `Bool::True`
values in the Pair shorthand form (`:a1`, `:a`) rather than the fully-spelled-out
`:a1(Bool::True)` form — even when the argument was written with explicit `=>` syntax rather than
colon-pair syntax. mutsu always renders the fully-spelled-out form for a slurpy-hash-collected
`Bool::True` pair.

Curiously, this abbreviation does **not** happen for a `Hash` built by direct literal/list
assignment with the same `Bool::True` values — both `raku` and mutsu print the full
`:a(Bool::True)` form there, and both already agree on `.raku` for a **standalone** `Pair` (not
inside a `Hash`) with a `Bool::True` value (`:a1`, in both). So the divergence is specific to
`Hash.raku`'s handling of pairs that arrived via slurpy-hash **argument binding** — something in
mutsu's named-argument-to-slurpy-hash collection path does not preserve whatever marker Rakudo
uses (possibly related to how `Capture`/signature-binding materializes named args as Pairs, vs.
how a hash literal materializes `=>` pairs) to make `Hash.raku` choose the shorthand form.

## Minimal repro

```raku
sub s(*%h){ say %h.raku };
s :a1:b2;
```
- `raku`: `{:a1, :b2}`
- `mutsu`: `{:a1(Bool::True), :b2(Bool::True)}`

Also reproduces with explicit `=>` syntax at the call site (ruling out a colon-pair-specific
marker):
```raku
sub s(*%h){ say %h.raku }; s a => True;
```
- `raku`: `{:a}`
- `mutsu`: `{:a(Bool::True)}`

Does **not** reproduce for a plain hash literal/list assignment (both sides agree, full form):
```raku
my %h = a => True, b => False, c => 1; say %h.raku;
# both raku and mutsu: {:a(Bool::True), :b(Bool::False), :c(1)}
```

## Affected files (starting point)

- `src/builtins/methods_0arg/raku_repr.rs` / `src/runtime/methods_raku_dispatch.rs` — `.raku`
  rendering for `Hash`/`Pair`
- Named-argument-to-slurpy-hash binding (wherever `*%h` collects the caller's named args into
  Pairs) — look for whatever distinguishes a signature-bound named-arg Pair from a hash-literal
  Pair, since that's the signal `Hash.raku` needs to consult to decide the abbreviated form.
