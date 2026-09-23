# Every `nqp::` op now documents its complexity, and the quadratic ones are tracked

Each op arm in the `nqp::` dispatch tables (`src/runtime/nqp_ops*.rs`), each
`NqpPure` body, and each control-flow form in `src/compiler/nqp_forms.rs`
now carries one comment in the same form:

```text
// Cost: O(n), n = chars of $s. MoarVM: O(1) -- see #9130.
```

The legend (what the variables mean, what is and is not counted, what
"amortized" relies on) is in `src/runtime/nqp_op_ids.rs` under "Complexity
annotations". The `MoarVM: O(..)` suffix appears only where mutsu's bound is
worse than MoarVM's for the same op, and every such line names an issue, so
`grep -rn 'MoarVM: O(' src/` is the list of known complexity deficits (38
arms at the time of writing).

`scripts/nqp-complexity-check.sh` checks those claims by measurement: it times
each op in a loop at N and at 2N and prints the ratio. A ratio near 2 means
the loop is linear and near 4 means it is quadratic. It is a manual
diagnostic, not a CI gate, because wall-clock ratios move with load.

## What the audit found

On a release build these loops turned out quadratic in mutsu, where rakudo
runs each of them in under 0.05 s at N = 40000:

- **#9129.** `nqp_char_cache` has a single slot, so `ordat`, `substr`, the
  `index` family, `iscclass`, `findcclass` and `eqat` pay O(n) on every call
  once a loop alternates two strings (ratios 3.3 to 4.8).
- **#9130.** `nqp::chars` copies and counts the string on every call (ratio
  4.26).
- **#9131.** `nqp::radix` collects the whole string whatever `$pos` is (ratio
  3.67).
- **#9132.** The Buf ops `writeuint` and `bindpos_i`/`splice` on a Buf
  decode and re-encode the whole buffer on each write, and `readuint`,
  `slice` and wide-buf `atpos_i` copy it on each read (ratios 3.2 to 4.2).
- **#9121.** `shift` was already tracked there; `unshift` was added to it.

The audit also turned up a correctness bug, #9133: `nqp::atpos_i` on a
buf16/32/64 returns only the low byte of an element. The smaller per-call
deficits are grouped in #9134: attribute-map clones in
`getattr`/`bindattr`, string copies where a refcount bump would do, and
uncached `istype`/`can` lookups. None of them makes a loop quadratic.
