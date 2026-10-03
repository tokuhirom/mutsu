# Entering and leaving a block no longer scales with the routine registry or the block's size

Every `BlockScope` (a bare block, or any block body that declares a lexical)
snapshotted the routine registry on entry and restored it on exit, so that a
`my sub` or `my token` declared inside stops being visible afterwards. Six of
the nine snapshotted tables were copied wholesale: the grammar tokens, the
`proto token` markers, the our-scoped subs' key set, and the import-alias
sets. The restore then walked the token and our-scoped tables and bumped the
token and method generations unconditionally. That retired every
generation-keyed method cache on each exit of a block that declared nothing
at all.

All nine tables are now copy-on-write `Arc`s, so the snapshot costs nine
refcount bumps. When every table is still the snapshot's own `Arc` on exit,
the block changed nothing the restore would put back, and the restore returns
at once. A block that does declare a routine still takes the full path, and
there the token loops (and the token-generation bump) are skipped when the
token tables were not touched.

Entry also no longer rescans the block's ops. The slots the block's
declarations own, its `state` slots and whether it binds a topic of its own
are now computed once per chunk in `CompiledCode::block_range_facts` (next to
the label and `state` indexes in `op_scan_index`). They used to be collected
by a scan of the whole block on every entry. The same applies to
`BlockLocalScope`.

`scripts/vm-complexity-check.sh` gains cases for this. With a fixed body of
5000 × `{ my $y = 1; $t += $y }`, doubling N from 500 to 1000 gives these
times (release build, `MUTSU_JIT=off`):

| grows with N | before | after |
| --- | --- | --- |
| `our sub`s in the registry | 0.039 → 0.060 s (1.55×) | 0.0074 → 0.0071 s (0.96×) |
| tokens of a grammar | 0.150 → 0.329 s (2.20×) | 0.0087 → 0.0084 s (0.97×) |
| untaken ops in the block | 0.021 → 0.030 s (1.45×) | 0.0072 → 0.0070 s (0.97×) |

Issue #9170 is still open for the other items: closure capture, `gather`,
the `ImportScope` snapshot, `RoutineScope`'s diff, and the sigilless-alias
sync.
