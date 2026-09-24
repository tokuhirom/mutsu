# `~=` appends in place for closure-captured variables and `$!attr`

#9141 made `$s ~= $x` append in place for a plain local. It grows the
existing buffer when nothing else holds the string. Two common shapes still
copied the whole accumulated string on every append, because the string was
always held twice:

- **A local captured by a closure** (`my $x = ""; my &g = { $x ~= "あ" }`).
  Inside the closure, `$x ~= ...` runs `AtomicCompoundVar`. That op locks the
  variable's shared cell for an atomic read-modify-write, but it *cloned* the
  old value out of the cell before concatenating. The cell and the clone both
  held the buffer. It now moves the value out under the same lock and appends
  with `str_appended_nfc`. Nothing between the take and the store can fail or
  run user code, so the cell is never observed empty, and concurrent
  `start { $x ~= ... }` appends still lose nothing.
- **A private attribute** (`method go { $!s ~= "あ" }`). `$!s` was never fused
  into `ConcatAssignLocal`: its slot is allocated lazily by
  `compile_expr_var`, so the fusion's `local_map` probe missed it. Even fused,
  `self`'s attribute cell mirrors the slot and holds the same string. The
  fusion now covers a private scalar attribute inside a method. Its in-place
  path moves the string out of the slot, drops the cell's reference under the
  attribute map's write lock, appends, and stores the result into both.

Both in-place paths are taken only when the store they skip would be the
identity for a `Str` result:

- no user `infix:<~>` or `infix:<~=>` is declared;
- the variable or attribute has no declared, `subset` or `where` constraint
  (a failed check has to leave the old value, which an in-place append has
  already consumed);
- the attribute slot has no env mirror.

Anything else takes the previous path unchanged.

`scripts/str-complexity-check.sh append`, release build, 4-core container:

| case | before | after |
|---|---|---|
| closure-captured, N=400k / 800k | 2.21 s / 6.69 s (ratio 3.02) | 1.04 s / 1.70 s (ratio 1.64) |
| `$!s ~=`, N=80k / 160k | 0.48 s / 1.93 s (ratio 4.05) | 0.051 s / 0.106 s (ratio 2.08) |

The closure case's remaining constant is the closure call itself, not the
append.

Pinned by `t/routines/call/concat-assign-captured-and-attr.t` (18 assertions,
identical under raku). The tests cover aliases keeping their value, subset
failures keeping the old value, undefined and non-Str operands, an `is rw`
alias, combining marks, and four threads appending through one captured cell.
With #9272 (strands for a shared operand of `~`, ADR-0120) this retires every
`#9209` suffix.
