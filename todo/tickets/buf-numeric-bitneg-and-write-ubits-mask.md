# Two pre-existing `Buf` parity gaps: `+^$buf` and `write-ubits` bit masking

Both found 2026-07-28 while smoke-testing the ADR-0015 P2 step 1 refactor
(`src/value/value_buf.rs`) against `raku`. **Neither is caused by that
refactor** — it left the arithmetic op dispatch and the bit-masking helpers
(`crate::builtins::buf_bits::write_bits`, `write_bits_into_bytes`) untouched, and
both gaps reproduce on the same code paths as before. They are recorded here
rather than fixed in that PR to keep it a pure refactor.

Both are also *outside* what the whitelisted `roast/S03-buf/read-write-bits.t`
covers — that file passes, which is why they went unnoticed.

## 1. `+^$buf` does not numify the buffer

`+^` (numeric bitwise negation) should numify its operand, and a `Buf` numifies
to its element count. mutsu numifies it to 0 instead, so the result is always
`-1`:

```raku
my $c = Buf.new(0xff, 0x00);
say $c.Numeric;   # 2   both
say +$c;          # 2   both
say (+^$c);       # raku: -3   mutsu: -1
```

`.Numeric` and prefix `+` are both already right, so the fix is narrow: the
numeric-bit-negate op needs to coerce through the same path they do. Note the
**string** bitwise negate `~^$buf` is correct (`Buf:0x<00 FF>` in both), and is a
genuinely different operation (`exec_str_bit_neg_op` in
`src/vm/vm_arith_ops.rs`), so do not "unify" the two.

## 2. `write-ubits` / `write-bits` at bit offset 0 keeps bits it should clear

Writing a bit field that does not fill the byte leaves the untouched low bits
alone in mutsu, while Rakudo clears them:

```raku
my $d = Buf.new(0x05, 0xAA);
$d.write-ubits(0, 4, 3);
say $d.gist;            # raku: Buf:0x<30 AA>   mutsu: Buf:0x<35 AA>
say $d.read-ubits(4,4); # raku: 0               mutsu: 5
```

At bit offset 4 the two agree (`Buf.new(0x05, 0xAA).write-ubits(4, 4, 3)` is
`Buf:0x<03 AA>` in both) — but only because the high nibble of `0x05` was
already 0, so that case cannot distinguish the two models. `write-bits` behaves
identically to `write-ubits` here in both implementations.

**Establish the intended model before changing anything.** It is not obvious
from the above whether Rakudo is specifying "the write zero-extends over the
whole byte(s) it touches" or whether this is a Rakudo quirk; check
`raku-doc/doc/Type/Buf.rakudoc` and, if it is silent, the nqp implementation.
Then extend `roast`-shaped coverage in `t/native-buf-mut.t` with a case whose
*untouched* bits are non-zero, which is exactly the coverage hole here.

Note there are **three** write-bits code paths that would all need the same fix,
which is itself worth collapsing:

- `src/runtime/methods_mut_dispatch.rs` (~line 382), via
  `crate::builtins::buf_bits::write_bits`;
- `src/runtime/methods_mut_dispatch.rs` (~line 2040), via a *separate* local
  `write_bits_into_bytes`;
- `src/vm/vm_call_method_mut_ops.rs` (~line 2620), the VM fast path.

Two independent implementations of the same masking is the reason a fix in one
place would look like it worked while another path stayed wrong.
