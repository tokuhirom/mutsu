# `CBOR::Simple`'s entire upstream test suite now passes (00-06, 341/341)

Building on the earlier sessions' fixes (a `use lib`-with-computed-path parser bug, a `my
constant` + `elsif` parser bug, a BigInt-negate-to-i64 downcast bug, and the anonymous-container
in-place-reassign aliasing bug that fixed `01-basic.rakutest`'s nested-array decode), this session
closed out every remaining failure across `CBOR::Simple`'s (0.1.4) upstream test suite. All seven
files now pass in full:

- `00-use.rakutest` — 1/1 (already passing)
- `01-basic.rakutest` — 74/74
- `02-malformed.rakutest` — 94/94
- `03-diagnostic.rakutest` — 75/75
- `04-tags.rakutest` — 39/39
- `05-malformed-tags.rakutest` — 23/23
- `06-typed-arrays.rakutest` — 36/36

Seven distinct, general interpreter bugs were root-caused and fixed, each unrelated to
`CBOR::Simple` itself:

1. **`Int ~~ EnumValue` smartmatch always returned False** (`pure_smart_match`,
   `src/vm/vm_smart_match.rs`). A plain `Int` smart-matched against a specific enum VALUE (not the
   enum type object) — the classic `given ($byte +& SOME_MASK) { when SOME_ENUM_CONST { ... } }`
   idiom — had no matching arm at all, so it fell through to the interpreter's generic
   string-equality fallback (comparing the stringified Int against the enum key's own NAME).
   Fixed by unwrapping an `Enum` value to its underlying value and recursing, on either side of the
   match — verified against real `raku`, this even correctly crosses enum types (`Red ~~ Apple` is
   True when both underlying values are 0). This broke `cbor-diagnostic()` universally (all 75
   subtests in `03-diagnostic.rakutest` failed) since its whole dispatch is a `given
   $major-type { when CBOR_UInt { ... } }` chain.

2. **The same gap in numeric `==`/`<`/`>`/etc** (`coerce_infix_operand_numeric`, the shared
   bridge for arithmetic/comparison operand coercion). A large `Int`/`BigInt` compared against an
   enum value outside `f64`'s exact-integer range (`18446744073709551615 == CBOR_Tag_Invalid_8Byte`)
   silently lost precision through the generic same-variant-mismatch float fallback. Fixed the same
   way: unwrap `Enum` to its underlying value before the numeric-bridge coercion.

3. **`nqp::writeuint`/`nqp::writeint` with a raw enum value argument wrote 0** (`to_u128_value`,
   `src/builtins/buf_write_int.rs`). No `ValueView::Enum` arm existed in the Value-to-`u128`
   converter used by the buffer-write ops, so `nqp::writeuint($buf, $pos, CBOR_Tag_Date_Integer,
   $ne8)` (CBOR::Simple's Date tag encoding) always wrote a 0 byte instead of the tag number.

4. **`nqp::istype($x, Nil)` always answered False** (`src/runtime/nqp_ops.rs`). The bare `Nil` value
   used as a TYPE argument is a `ValueView::Nil`, not a `Package("Nil")` type object like other
   builtin types — the `istype` op's type-name extraction only recognized `Package`/`Instance`
   arguments. This broke CBOR::Simple's absent-value encoding, which checks `nqp::istype($_, Nil)`
   on array elements bound to `Nil` via `BIND-POS`.

5. **Native `num32` scalars never truncated to float32 precision** (`exec_type_check_op_inner`,
   `src/vm/vm_misc_typecheck.rs`; plus the reassignment path in
   `wrap_native_int_by_constraint`/`src/vm/vm_var_assign_local.rs`). `my num32 $x = EXPR` stored the
   value at full 64-bit precision untouched, in BOTH statement form and — critically —
   expression-context form (`f((my num32 $x = EXPR))`), the exact shape `CBOR::Simple`'s float
   encoder uses to decide "can this Num safely round-trip through a 4-byte CBOR float?"
   (`nqp::iseq_n($_, (my num32 $num32 = $_))`). Without truncation that check was always true, so
   every double got wrongly encoded as a lossy 4-byte float. The fix lives in the `TypeCheck`
   opcode handler — the one place BOTH declaration forms compile through (the native-int branch
   right above it already mutated the stack value the same way; `num32` just never had the
   equivalent). This alone fixed 3 of the 4 previously-known "pre-existing float" failures in
   `01-basic.rakutest` (tests 24/27/28) and the one remaining `04-tags.rakutest` failure.

6. **`Blob[uint16]` (`blob16.new(...)`) UTF-16-decoded a surrogate pair incorrectly** (three
   `is_wide` checks: `src/builtins/mod.rs` ×2, `src/runtime/methods_io_dispatch.rs`). Each checked
   an explicit name list (`"utf16" | "buf16" | "Buf[uint16]"`) that missed the CAPITALIZED `Blob`
   spelling `blob16.new(...)` actually produces. So each 16-bit code unit silently truncated to its
   low BYTE before UTF-16 decoding, turning `blob16.new(0xd800, 0xdd51).decode('utf-16')` (should be
   U+10151) into a completely different, wrong BMP codepoint (U+5100) — this was the 4th
   "pre-existing unicode" failure (test 53), mis-attributed to unicode/CJK handling when the actual
   literal was a UTF-16 surrogate pair decode via `blob16`, not a source-text character at all.
   Fixed by switching all three checks to the existing general `buf_elem_width()` helper (which
   already correctly recognizes every 8/16/32/64-bit spelling by substring, used elsewhere and
   already unit-tested for exactly this).

7. **`array[EXPR]` rejected a runtime-computed type-parameter expression** (`is_non_parametric_type`,
   `src/runtime/runtime_class_query.rs`). Lowercase `array` (the native shaped/typed array
   declarator used as a term) was missing from the parametric-builtin allowlist that only had
   capitalized `Array`. The COMPILER already special-cased the compile-time-literal spelling
   (`array[num32]` synthesizes its type name directly at compile time), so that narrow case worked
   — but `array[$is-signed ?? int8 !! uint8].new` (RFC 8746 typed-array decoding, picking a
   signed/unsigned element type at runtime) fell through to the VM's generic Package-indexing path
   and threw `X::NotParametric` ("array cannot be parameterized"). This was originally assumed to be
   a much larger "implement parameterized array types" feature gap; it turned out to be a one-line
   allowlist omission — the rest of the machinery (native storage, `.of`, decode dispatch) already
   worked correctly for the literal case and just needed the runtime path to route through it too.

8. **Mismatched out-of-bounds-read error message broke every `CATCH`-based "truncated input"
   test** (`mvm_array_read_buf_oob_message`, new shared helper in `src/builtins/mod.rs`, used by
   `nqp::readuint`/`readint`/`readnum` in `src/runtime/nqp_ops.rs` and the `.read-uint16`-style
   `Buf`/`Blob` methods in `src/builtins/methods_narg/buf.rs`). MoarVM raises the exact wording
   `"MVMArray: read_buf out of bounds offset ... start ... elems ... count ..."` for every
   byte-addressed buffer read past the end; `CBOR::Simple` (and other MoarVM-op-based decoders)
   match this by PREFIX in their own `CATCH { when /^ 'MVMArray: read_buf out of bounds' / { ... }
   }` to turn a low-level truncated-read error into their own typed `X::Malformed` exception.
   mutsu previously raised THREE different, non-matching messages across these call sites
   (`"nqp::readuint: read of N bytes at offset M past end"`, a bit-offset-flavored `"read from out
   of range. Is: X, should be in 0..Y"`, ...), so the `when` never matched and the raw low-level
   error always leaked out instead — this alone accounted for 38 of `02-malformed.rakutest`'s 94
   subtests (every "End of input" / truncated-string / truncated-array test).

## Testing

New regression tests: `t/enum-value-numeric-compare.t`, `t/native-num32-truncation.t`,
`t/blob16-utf16-surrogate-decode.t`, `t/array-dynamic-parameterization.t`,
`t/buf-read-oob-mvm-message.t`, plus 3 new cases appended to `t/nqp-cbor-ops.t`. Every new test was
verified against real `raku` and matches its output exactly.

## Follow-up filed separately

`todo/tickets/dynamic-array-parameterization-push-wrap.md`: a dynamically-parameterized
`array[EXPR].new` array (fix #7 above) does not apply native-width wrapping on `.push` of an
out-of-range value (unlike a statically-declared `my uint8 @a`), because the wrap logic is keyed
off the lexical variable's compile-time type constraint rather than the array value's own element
type. Not needed by `CBOR::Simple` (its decoder never pushes an out-of-range value), so left as a
narrower follow-up rather than folded into this session.
