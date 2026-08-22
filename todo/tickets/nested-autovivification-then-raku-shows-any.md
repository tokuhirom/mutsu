# A chained hash-then-array autovivification leaves the root variable showing `Any` on `.raku`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/subscripts.rakudoc:418`).

## Repro

```raku
my $beatles;

$beatles{"White Album"}[0] = "Back in the U.S.S.R.";  # autovivification!

say $beatles.raku;  # OUTPUT: «${"White Album" => $["Back in the U.S.S.R."]}␤»
```

- raku: `${"White Album" => $["Back in the U.S.S.R."]}`
- mutsu (`target/debug/mutsu`): `Any`

## Analysis

`$beatles` starts undefined. Indexing it with `{"White Album"}` should autovivify it into a Hash
container, and further indexing that with `[0]` should autovivify that hash value into an Array
container, with the final assignment landing in `$beatles{"White Album"}[0]`. mutsu's `.raku` on
`$beatles` afterward shows `Any` — meaning either the autovivification chain never actually wrote
back into `$beatles`'s own container (the write only affected a disconnected temporary), or the
two-level `{...}` → `[...]` autovivification chain isn't establishing the container nesting at
all.

## Affected files (starting point)

- `src/vm/vm_var_ops.rs` — indexing/autovivification for chained hash/array subscripts
- Compare with a single-level autovivification (`$h{"k"} = 1` alone, or `$a[0] = 1` alone) to see
  whether those work and only the *chained* two-level case (hash-of-array) fails.

## Suggested next step

`--dump-ast` the assignment to see how the chained `{...}[...]` index-assign is compiled, and
trace whether the intermediate autovivified Hash container is the same object that
`$beatles`'s own scalar container ends up referencing.
