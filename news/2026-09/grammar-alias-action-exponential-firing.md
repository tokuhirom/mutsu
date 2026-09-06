# An aliased capture's grammar action fired 2^depth times

`<x=rule>` — a non-suppressing capture alias — files the match under **both**
the alias name and the rule's own name, which is what Rakudo does (`$<x>` and
`$<rule>` both answer, and they are the *same* cursor: `$<x> === $<rule>` is
`True`). mutsu stored two **independent** copies of the capture node, the
second a deep clone of the whole matched subtree, and the grammar action walk
then dispatched each slot on its own. So every aliased capture ran its
subtree's actions twice — and because the duplication happens at every level,
nested aliases multiplied it: a leaf action fired `2^depth` times.

The minimal repro (now pinned by `t/grammar-alias-action-fires-once.t`,
verified against rakudo 2026.07):

```raku
grammar E3 {
    token TOP { <x=l1> }; token l1 { <y=l2> };
    token l2 { <z=l3> };  token l3 { 'x' }
}
```

raku fires each of `l1`/`l2`/`l3` once. mutsu fired them 2/4/**8** times.

This is a correctness bug first — an action method with side effects ran up to
`2^depth` times — and a large performance one second. `benchmarks/bench-yaml-parse.raku`
exercises it hard: YAMLish's quoted-scalar rules alias `<str=single-bare> |
<str=single-quotes> | <str=space>` and sit eight alias levels deep, so the
`space` action fired **256 times per matched space character** and the parse
ran 40115 action methods where rakudo runs 997.

## The fix

Both slots now push the **same** `Arc<CapNode>` — no subtree clone, and the
two names really are one cursor (mutsu now answers `True` for `$<x> === $<rule>`
too). The action walk dispatches at most once per capture node per parent
(`Value::match_node_identity` compares the stored node behind two lazy
`Match`es) and hands the second slot the same updated Match, so the `.made`
the single dispatch produced still reaches both names.

## Effect

On `benchmarks/bench-yaml-parse.raku` (release, idle box, median of 7 runs):
1.14s → 0.34s. Counted rather than timed, the same run's `MUTSU_VM_STATS`:

| counter | before | after |
| --- | ---: | ---: |
| opcodes executed | 75595 | 2877 |
| `dual-store: env_deep_copies` | 41360 | 580 |
| `regex-captures: match_materializations` | 1743 | 69 |
| grammar action methods run | 40115 | 191 |

Found by comparing per-action fire counts against rakudo on an instrumented
copy of YAMLish, after a callgrind profile showed `invoke_leaf_action_lazy`
running 76800 times on a document with ~150 leaf captures. See
`todo/perf/yaml-parse-throughput.md` round 10.
