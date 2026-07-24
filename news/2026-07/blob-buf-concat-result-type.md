# `Blob ~ Buf` widens to `Buf` instead of keeping the LHS type

`infix:<~>` on two buffer values preserved the *left* operand's type
unconditionally. Rakudo instead types the result by whether the two operands
have the **same** type: matching types are preserved, any mismatch widens to the
plain mutable `Buf`.

```raku
say (Blob[uint8].new(1) ~ Blob[uint8].new(2)).^name;  # Blob[uint8]  (same)
say (Buf[uint8].new(1)  ~ Buf[uint8].new(2)).^name;   # Buf[uint8]   (same)
say ("a".encode ~ "b".encode).^name;                  # utf8         (same)
say (Blob[uint8].new(1) ~ Buf[uint8].new(2)).^name;   # Buf          (mismatch)
say (Blob[uint8].new(1) ~ Blob[uint16].new(2)).^name; # Buf          (mismatch)
```

mutsu reported `Blob[uint8]` for the last two.

## Why the distinction is load-bearing

It is what lets a `Blob`-typed accumulator become bindable to a `Buf` once
something mutable has been appended to it:

```raku
my Blob[uint8] $acc = Blob[uint8].new;
$acc ~= Buf[uint8].new(0x41, 0x42);   # container now holds a Buf
my Buf $chunk = $acc.clone;           # ... so this type-checks
```

`HTTP::UserAgent` is built on exactly this shape. `get-response` accumulates the
socket reads into `my Blob[uint8] $first-chunk` via `~=` — and `recv(:bin)`
hands back a `Buf` — then slices the body out with `.subbuf` and passes it to
`get-chunked-content`, whose first statement is `my Buf $chunk = $content.clone`.
With the LHS type preserved, the value was still a `Blob[uint8]` there and the
assignment died with *"Type check failed in assignment to $chunk; expected Buf,
got Blob[uint8]"*, so no chunked response body could be read.

## Fix

In `concat_values` (`src/vm/vm_coerce_concat_ops.rs`), compare the two operands'
class names: equal names keep that name, anything else produces `Buf`. Byte
concatenation itself is unchanged.

Pin: `t/blob-buf-concat-result-type.t` — 12 assertions that pass identically
under `raku` and mutsu.
