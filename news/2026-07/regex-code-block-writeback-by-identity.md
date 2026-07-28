# A regex code block's writeback is detected by binding identity, not by Debug text

Every regex `{ … }` block ran through `eval_regex_code_block_body`, which
answered "which enclosing lexicals did this block write?" by **formatting the
entire env with `{:?}` before and after the block and comparing the strings**.

That made `core::fmt` the single largest cost of a grammar parse. A `perf`
profile of `load-yaml` over a 16-line YAML mapping (the bundled `YAMLish`
battery) attributed ~20% of the run to formatting machinery alone —
`String::write_str` 10.2%, `Debug::fmt` 5.0%, plus `DebugStruct::field`,
`DebugMap::entry`, `pad_integral` and `fmt::write` — before counting the
`malloc`/`memmove` churn that feeds it. A YAML grammar runs a code block per
line and per backtrack, and each run formatted a whole module's env twice.

The comparison is now `Value::same_binding()` — an O(1) NaN-box word compare that
already existed for exactly this purpose. The snapshot holds cloned `Value`s (an
`Arc` bump), which also keeps the old allocation alive so a freed address cannot
be recycled into a false "unchanged".

Identity is also the *right* question. What has to be pushed back into the
caller's local slot is a name that was **rebound**; a container mutated in place
keeps its allocation, which the caller's local already shares, so it needs no
writeback. And the old text comparison silently missed the opposite case — a
rebinding to a different value whose `Debug` output happened to match (`my $x =
'1'` then `$x = 1`) — which is now reported.

## Effect

Release build, `load-yaml` over a synthetic `k$_: v$_` block mapping:

| lines | before | after | raku |
| --- | --- | --- | --- |
| 16 | 1127ms | **568ms** | 196ms |
| 64 | 10065ms | **2147ms** | 442ms |

4.7x faster at 64 lines, and the growth is now essentially linear (4x the input
costs ~3.8x the time, where it used to cost 9x). Whole upstream YAMLish files:
`roundtrip` 10.0s → 5.9s, `test-harness` 24.1s → 18.6s, `anchor-alias` 1.4s →
0.6s, `p5-tests` 1.7s → 0.5s.

`basic.rakutest` moved only 45.5s → 43.6s, so its documents are dominated by a
different cost — the next lead, kept in `todo/tickets/yaml-parse-throughput.md`.

Pin: `t/regex-code-block-writeback.t` (assignment, rebinding to a look-alike
value, in-place container mutation, several blocks in one match) — green under
`raku` too.
