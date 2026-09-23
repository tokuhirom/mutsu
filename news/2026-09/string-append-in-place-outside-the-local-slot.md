# String appends stay linear outside the plain local slot

`$x ~= $y` was amortized linear only on the fused `ConcatAssignLocal` path of a
plain local (#8695, #8725). Every other way of building a string by appending
fell back to the general `Concat`, which copied both operands and then ran a
full NFC pass over the whole accumulated result whenever it was not ASCII — so
20,000 appends of one kana into a hash element took 4.5 s, against rakudo's
0.02 s ([#9141](https://github.com/tokuhirom/mutsu/issues/9141)).

Four changes. Timings are `scripts/str-complexity-check.sh append concat`,
t(2N) at N = 10,000 appends of `"あ"` on a release build in a 4-core
container; the "before" column is the measurement #9141 was filed with (a
different session, so read it as an order of magnitude), the ratio column is
t(2N)/t(N) at N = 100,000 (`SCALE=10`) after the change, where ~2 is linear:

| case | before | after | ratio at 100k |
|---|---:|---:|---:|
| `%h<k> ~=` | 4.53 s | 0.045 s | 1.9 |
| `@a[0] ~=` | (not filed) | 0.039 s | 2.0 |
| inside `given`/`when` | 4.60 s | 0.0074 s | 2.2 |
| `$y = $y ~ "あ"` | 4.65 s | 0.0047 s | 2.0 |

- **`concat_values` normalizes the join, not the result.** A plain `Str` left
  operand now appends through the same primitive the fused local op uses
  (`Value::str_appended_nfc`): its buffer is grown in place when nothing else
  holds it, and NFC is restored by classifying the right operand and redoing a
  bounded window at the join. That alone took the non-ASCII fallback from
  seconds to tens of milliseconds, since the whole-string NFC pass, not the
  copy, was the cost. `x` on a non-ASCII operand skips its NFC pass the same
  way when the operand is NFC and begins at a normalization boundary.
- **`%h{$k} ~= rhs` / `@a[$i] ~= rhs` store in place.** The element store
  (`IndexAssignExprNamed`) gained a `concat_append` flag and does the
  concatenation itself. When the element read onto the stack is still the
  very allocation in the container, it stores an empty string through the
  ordinary fast store lane (releasing the container's reference), grows the
  now-unique buffer, and stores the result through the same lane — so every
  store rule the lane enforces stays the lane's, and a store it would decline
  falls back to the old sequence untouched.
- **`$y = $y ~ rhs` fuses like `$y ~= rhs`.** `ConcatAssignLocal` carries a
  flag for whether the general path seeds an undefined LHS with `''` (the
  METAOP_ASSIGN form) or warns (the literal `~`). Reading the LHS after the RHS
  matches rakudo, which passes the container to `~`: `$z = $z ~ do { $z = 'q';
  'r' }` is now `qr`, as there, instead of `ar`.
- **A local mirrored to env appends in place too.** A slot that also lives in
  env (declared inside `given`/`when`) used to decline the fast path because
  the mirror held a second reference; when the mirror is the same allocation
  it is cleared alongside the slot, and the result goes through the real
  `SetLocal`, which updates both.

Two shapes still copy per append — a local captured and mutated by a closure
(held in a shared cell) and `$!attr ~=` — though without the NFC pass they are
~70x faster than before. They are tracked in
[#9209](https://github.com/tokuhirom/mutsu/issues/9209).

Pinned by `t/types/string/concat-append-in-place-elements.t`.
