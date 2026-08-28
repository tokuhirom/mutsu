# `eq`/`ne` between two Blobs now compares bytes across Blob types

`"hi".encode eq Buf[uint8].new(104, 105)` answered `False` in mutsu where rakudo
answers `True`, and `ne` was wrong in the same way. The two operands hold the
same two bytes; they differ only in their Blob type (`utf8` vs `Buf[uint8]`).

## The rakudo rule, measured

Rakudo's comparators carry a `(Blob:D $a, Blob:D $b)` candidate
(`SETTING::src/core.c/Buf.rakumod:1776`). For `eq`/`ne` it compares the **bytes**
whatever the two Blob types are — verified directly:

```
$ raku -e 'my $u = "hi".encode; my $b = Buf[uint8].new(104,105);
           say $u eq $b, " ", $b eq $u, " ", $u ne $b, " ",
               $u eq Blob[uint8].new(104,105)'
True True False True
```

The `.Str` coercion only applies when the *other* side is not a Blob, and there
`utf8` is the one Blob type with a working `.Str`: `"hi".encode eq "hi"` is
`True`, while a plain `Buf[uint8]` in a string context dies with
`X::Buf::AsStr`.

Ordering is a separate rule and was deliberately left alone: rakudo's
`lt`/`gt`/`le`/`ge`/`cmp` Blob candidates are effectively same-type only, so
`"hi".encode lt Buf[uint8].new(104,105)` is a type-check failure in rakudo where
mutsu answers `False`. That divergence, and the missing `X::Buf::AsStr` on
`Buf eq Str`, are recorded in
`todo/tickets/blob-comparison-should-die-instead-of-answering.md` together with
the dozen other `to_str_context()`-based comparison sites they would have to
move with.

## Root cause

Every string comparator in `src/vm/vm_comparison_order_ops.rs` already had the
right byte branch — `exec_str_eq_op` and friends all test
`is_buf_value(&l) && is_buf_value(&r)` and call `buf_cmp_bytes`. The branch was
simply unreachable for a mixed pair.

`coerce_str_compare_operands` applied `decode_utf8_compare_operand` to each
operand **independently**, before the comparator body ran. That turned the
`utf8` side into a decoded `Str`, so the pair stopped being a Blob pair and fell
into the `to_str_context()` else-branch — where the surviving `Buf[uint8]`
stringifies to its *gist*, `Buf[uint8]:0x<68 69>`. Comparing `"hi"` against that
text is `False` for every input, which is exactly the symptom. The same
mechanism made `"".encode eq Buf[uint8].new()` answer `False`.

The utf8 decode is a property of the **pair**, not of either operand on its own.
The fix decides it jointly: when both operands are Blob values the decode is
skipped and the existing byte branch is reached; otherwise both operands are
decoded exactly as before, preserving `"hi".encode eq "hi"`. Two `utf8`s now
also route through the byte comparison rather than the decoded-text one, which
is the same answer — `.encode` produces NFC bytes in both implementations, and
UTF-8 byte order matches codepoint order.

## Effect

Pinned by `t/blob-comparison-across-types.t`, a 33-assertion matrix over
utf8/Buf/Blob in both operand positions, differing bytes, differing lengths,
empty buffers, multi-byte content and utf8-vs-`Str`. Every assertion is green
under real `raku` as well as under mutsu, which is what proves the file encodes
rakudo's semantics rather than mutsu's.

This closes two of the regressions in the `MUTSU_REAL_TEST=1` campaign
(`todo/deep/vendor-real-test-module.md`), where the strict Raku-level `Test`
module exposes divergences the lenient native Rust `is` hides:

- `roast/S32-io/slurp.t` — test 12, "binary slurp returns correct content"
  (`is slurp($path, :bin), $test-contents.encode`: a `Buf` against a `utf8`).
- `roast/S32-io/spurt.t` — tests 1, 4, 12 and 15, "spurting Buf ok" /
  "spurting Buf with append" (the helper sub runs twice, once per path form).

Both files pass under both providers now; they already passed under the native
provider before, and continue to.
