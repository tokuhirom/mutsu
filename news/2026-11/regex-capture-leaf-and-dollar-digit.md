# A capturing regex in a loop stops paying for a phantom `state` and for leaf captures it throws away

Two costs dominated `bench-regex-capture` that had nothing to do with matching.

**A `state` variable nobody declared.** `$acc += $0` inside a block made the
parser declare an anonymous `state` variable for that block. The assignment
parser tried `$0` as an assignment target, `var_name` read it as a bare `$`
(it has no arm for a digit) and the statement parser minted a per-occurrence
anonymous-state name for it. The attempt failed, but the name stayed recorded
in the scope, so the block got an implicit `state` declaration. A loop then
reset it on every iteration. That reset builds the state's string key and
hashes it with SipHash, about 3.5k instructions per iteration. A digit after
the `$` now means a match variable, not a bare `$`, and nothing is minted
(`bare_dollar_is_assign_target`).

**A leaf node allocated on every close of a capture group.** The compiled
regex engine closed a capture group whose body captures nothing (`( \w+ )`) by
allocating a span-only `CapNode`. A match that backtracks closes such a group
once per attempt, and rewinding throws all but the last one away: about 86
allocations per failing match of `/ ( \w+ ) '=' ( \w+ ) '=' ( \w+ ) '=' /` on a
70-char line. The group now records a span-only slot. Such a slot renders as
the same lazy Match over a span-only node, built when the slot is read.
Previously a span-only slot rendered as an eager `Instance`, which was dearer
to build and to read.

`scripts/bench-det.sh`, release build, `main` `3c45be40` → the `$0` fix → both:

| benchmark | instructions | allocations |
|---|---|---|
| bench-regex-capture | 421.7M → 390.6M → 356.6M (-15.4%) | 374.8k → 351.3k → 214.1k (-42.9%) |

The other regex and grammar rows are unchanged, within ±1%.
