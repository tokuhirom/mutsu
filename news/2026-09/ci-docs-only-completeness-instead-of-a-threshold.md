# A 300-file threshold stood in for a question CI could just ask

The documentation-only classifier refused to classify any diff over 300 files:

```sh
# Guard against a huge diff: pagination or the API cap could truncate it, and a
# truncated list must never read as "docs only".
if [ "$count" -gt 300 ]; then
  echo false
  exit 0
fi
```

The property it protects is real. `classify` reads a list of changed paths and
says "all of these are documentation"; if the list is short of the diff, the
missing path could be `src/vm/vm.rs`, and the five build jobs would be skipped
for a code change that has never been compiled. A list that might be truncated
must not be classified.

The threshold was not that test, though. It was a guess about where truncation
starts, and it was wrong by an order of magnitude: `GET /pulls/{n}/files` serves
**3000** files, so 300 refused ten times more diffs than could possibly be short.

The bill came from the ecosystem sweep. A full-corpus run writes about 1300
records under `ecosystem/dists/`, every one of them on the allowlist precisely
because the sweep *measures* mutsu and cannot change it — and every such data PR
tripped the threshold and paid for `test`, `lint-configs`, `wasm-e2e`,
`gc-stress` and `jit-stress` to confirm that recording what mutsu did does not
change what mutsu does. Roughly 25 minutes on the critical path of a PR whose
entire content is numbers mutsu produced.

## Ask instead

A pull request states its own `changed_files`, so completeness is an equality
rather than an estimate, and it is exact at every size:

```sh
list_is_complete() { # list_is_complete <event> <api count> <count we read>
```

1269 read of 1269 is complete and merges in seconds; 3000 read of 4200 is short
and classifies as `false` exactly as before. A `push` has no such number to
compare against — the compare endpoint answers with at most 300 files and does
not say how many it left out — so there, and only there, a count is still the
whole signal: under 300 nothing was dropped, at 300 something may have been.

Both fail-safe defaults are unchanged and now rest on something derived: an
unreadable or short list is `false` for docs-only and `true` for the Miri
gate's `--gc-value`, because a skipped soundness check is a silently-unchecked
merge while a needless one only costs runner minutes. Twelve cases in the
script's self-test pin the predicate, including the 3000-file ceiling, a
missing API answer, and both sides of the push ceiling.

The lesson is smaller than the saving: when a guard is standing in for a
question, check whether the question has an answer. This one was one API field
away.
