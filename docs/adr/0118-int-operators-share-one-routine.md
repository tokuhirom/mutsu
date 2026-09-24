# ADR-0118: Every form of an integer operator, and `.succ`/`.pred`, is one routine

- **Status**: Accepted (2026-09-24, user decision: audit every non-`Str` family for the
  duplication ADR-0117 removed from strings, and unify what is found).
- **Deciders**: tokuhirom, Claude
- **Context**: [ADR-0117](0117-str-methods-and-nqp-ops-share-one-routine.md) (the same decision
  for `Str`), [ADR-0071](0071-native-operators-are-dispatch-candidates.md) (a user `multi` may
  out-narrow a builtin operator).

## 1. Context

A Raku integer operator has many surface forms -- the infix (`7 div -2`), its routine
(`&infix:<div>`), the reduction and triangle (`[div]`, `[\div]`), the hyper (`»div«`), the
reversed metaop (`Rdiv`) -- and some have method twins (`.abs`, `.succ`) or sub twins (`abs()`).
In rakudo they all bottom out in one candidate of `&infix:<div>`, one `Int.succ`.

mutsu implemented them per layer: the VM opcode (`vm_arith_int_ops.rs`, `vm_bitwise_ops.rs`), the
reduction fold (`ops_reduction.rs`, which the metaop and routine forms share), the methods
(`dispatch_core_math.rs`, `dispatch_core_numeric.rs`), the `abs()` builtin
(`functions/dispatch_1arg.rs`), and for `++`/`--` two more copies (`increment_value` in the VM and
`increment_mut_target_value` for `.=succ`). The copies had drifted, measured against rakudo:

| expression | rakudo | mutsu before |
|---|---|---|
| `$min div -1` (`$min` = i64::MIN) | 9223372036854775808 | **Rust panic** (`div_floor` overflow) |
| `$min mod -1` | 0 | **Rust panic** |
| `abs($min)` | 9223372036854775808 | **Rust panic** |
| `[div] 7, -2` | -4 | -3 (Euclidean) |
| `-$min` | 9223372036854775808 | 9.223372036854776e+18 (a Num) |
| `$min.abs` | 9223372036854775808 | -9223372036854775808 (wrapped) |
| `9223372036854775807.succ` | 9223372036854775808 | -9223372036854775808 (wrapped) |
| `(2**70).succ` | 1180591620717411303425 | 1180591620717411303424 (no-op) |
| `abs(-2**70)` | 1180591620717411303424 | 0 |
| `"²".succ` | `³` | `²` (while `++` gave `³`) |
| `$x = "a"; $x .= pred` | Failure | `"a"` (while `--` gave a Failure) |
| `5.5 +& 3` | 1 | 0 (while `[+&] 5.5, 3` gave 1) |
| `7.5 mod 2` | 1.5 | 1 (while `[mod] 7.5, 2` gave 1.5) |

## 2. Decision

1. **`src/builtins/arith/` is the single home** of the Raku-level integer operators:
   `int_div` (floored, `Int()`-coerced operands, BigInt on overflow, a `Failure` on a zero
   divisor), `arith_mod` / `int_mod_i64` (`mod` is `%` on the numeric values), `int_bitop`
   (`+&` `+|` `+^`), `int_shift_left` / `int_shift_right`, `int_negate` / `int_abs` (i64::MIN
   promotes), and `value_succ` / `value_pred` (a number's successor is literally
   `arith_add($n, 1)`; strings step through `str_increment`, superscript digits included).
2. **Every form calls it**: the VM opcodes, the reduction fold (and through it the routine,
   hyper, triangle and `R` forms), the methods, the `abs()` builtin, `++`/`--` and `.=succ` /
   `.=pred`. The VM keeps only what is genuinely its own: the ADR-0071 user-candidate check, the
   junction threading and the `throw_if_failure` operand check.
3. **Native semantics stay separate by contract.** `nqp::add_i` and friends, and the native
   `int` arithmetic, *wrap*; they live in `runtime::nqp_pure` and are not Raku `Int` operators.
4. **Enforced, not requested**: `scripts/check-str-prims.sh` from ADR-0117 is generalized to
   `scripts/check-prims.sh` (`make check-prims`, a `make test` prerequisite and a CI step). Its
   `int` rule fails on a hand-written `Integer::div_floor` / `mod_floor` outside
   `src/builtins/arith/` (opt-out: `int-prim: allow`), and its `names` rule bans the deleted
   copies (`shift_{left,right}_{i64,bigint}`, `superscript_{succ,pred}`) by name.
   `t/types/numeric/int-operator-forms-parity.t` pins 43 rakudo-measured values across the forms.

### 2.1 The native `nqp::*_i` / `*_n` ops

The same rule applies one level down. `nqp::add_i` and friends had their own bodies in two
executors -- the interpreter's op tables (`runtime::nqp_pure::eval`) and TRIR's typed ops
(`trir/exec.rs`) -- plus a third copy of the op *names* in each of TRIR's lowering table and the
JIT's inline whitelist. They disagreed: `nqp::bitshiftl_i(1, 64)` was `i64::MIN` in the
interpreter (count clamped to 0..63) but 1 under TRIR (count masked to six bits, which is what
MoarVM answers), so the result depended on whether the enclosing routine had been compiled.

- `runtime::nqp_native` holds the scalar body of each op (`add_i` ... `shl_i`, `shr_i`,
  `div_i`, `mod_i`, `cmp_i`, `cmp_n`), with MoarVM's semantics; `nqp_pure::eval` and every
  TRIR op call it.
- TRIR's `nqp_form` and the JIT's `NqpIntOp` are keyed on `NqpPure` (`nqp_pure::by_name` /
  `pure_op`), not on their own lists of names. TRIR's private `unbox_i` lowering (which read
  `3.7e0` as 3 where the op answers 0) is dropped; `nqp::unbox_i` reaches the op.
- `make check-prims` gains a `native` rule: no hand-written `.wrapping_*(` in the nqp tables,
  the VM's nqp path or TRIR's runtime outside `nqp_native.rs` (opt-out: `native-prim: allow`).
  `t/fixtures/trir-int-ops.raku` pins the shift counts with TRIR on and off against rakudo.

### 2.2 Positional access

The list family had the same shape. The positional `nqp::` ops each resolved their index on their
own (`nqp_ops.rs`, `nqp_ops_text.rs`, `nqp_ops_list.rs`, `nqp_ops_builtin.rs`, TRIR's
`trir_atpos_i`), and all of them clamped a negative index to 0 or read it as absent, so
`nqp::bindpos($l, -1, $v)` overwrote the *first* element. The Raku-level methods had drifted from
their subscripts: `@a.AT-POS(-1)` was `Nil` where `@a[-1]` is an `X::OutOfRange`, `my Int @i;
@i.AT-POS(5)` was `Any` where `@i[5]` is `Int`, `"abc".AT-POS(1)` indexed a character, and
`ASSIGN-POS` rebuilt the array, so every gap it grew claimed to `:exists`.

- `runtime::nqp_backing` holds `resolve_index` (MoarVM's VMArray rule: negative counts from the
  end, before the start dies), `elem_at`, `bind_elem` and `atpos_i`. Every `atpos*`, `bindpos*`,
  `splice` and TRIR's `AtPosI` go through them. `bindpos_i`/`_n` now convert the value they
  store, as `bindpos_s` and `push_*` already did.
- `.AT-POS($i)` on an Array/List or Str is `Interpreter::builtin_at_pos`, which runs the CORE
  `postcircumfix:<[ ]>` (`core_subscript`) itself.
- `ASSIGN-POS` and the `[]=` opcode's fast lane share `ArrayData::store_element`. The store is in
  place through the shared node, and a grown gap is a hole.
- `t/vm/nqp-list-index-parity.t` pins 27 rakudo-measured rows. There is no pattern rule for this
  family: `.max(0) as usize` has too many legitimate uses (byte offsets) to ban.

### 2.3 Object-level `nqp::` ops

Five object ops had their own copy of a question the VM already answers:

| op | answered with | should be |
|---|---|---|
| `nqp::istrue` | `Value::truthy()` (ignores a user `Bool`, a pending `.grep`) | `eval_truthy` (`?`, `if`, TRIR `TruthyObj`) |
| `nqp::istype` | `type_matches_value` (no smiley, enum, or `is Mu` MRO correction) | `type_object_accepts` (`~~`'s type-object arm, now a function) |
| `nqp::isconcrete` / `defined` | `value_is_defined` (a `Failure` and `Empty` read 0) | `value_is_concrete` |
| `nqp::eqaddr` | `values_identical`, i.e. `===` (user `WHICH`, memoized only after a `===`) | `values_same_object` |
| `nqp::clone` | the same instance back; an `Array` as a `List` | the native `Mu.clone`; `Value::array_shallow_clone`, shared with `.clone` |

`t/vm/nqp-object-ops-parity.t` pins 21 rakudo-measured rows. One difference is known and left
alone: MoarVM's `nqp::clone` copies attribute *slots*, so an `is rw` attribute's Scalar container
is shared with the original. mutsu does not store attributes as containers and cannot express that
sharing.

### 2.5 Character classes

In Rakudo, the regex classes are MoarVM's `CCLASS_*` table, the same one `nqp::iscclass` reads:
`\d` is `CCLASS_NUMERIC`, `\w` is `CCLASS_WORD`, `\s` is `CCLASS_WHITESPACE`, and `\n` (also
inside `<[...]>`) and `\N` are `CCLASS_NEWLINE`. `<alpha>` and `<alnum>` are `CCLASS_ALPHABETIC`
and `CCLASS_ALPHANUMERIC` plus `_`, and `<upper>`, `<lower>`, `<xdigit>`, `<blank>`, `<cntrl>` and
`<punct>` are their classes. Probing every class over U+0000..U+3000 and three astral blocks
confirms this exactly. mutsu's regex engine used Rust's `char` predicates instead. `\d` was ASCII
only (290 digits missed), and `\w` / `<alpha>` / `<alnum>` used `char::is_alphanumeric`, which
admits `No`, `Nl` and combining marks (about 1150 extra codepoints). `\n` also missed VT, FF and
U+2029.

- The table moves from `nqp_ops_text.rs` to `builtins::cclass`, with named constants and the
  `is_word` / `is_digit` / `is_space` / `is_newline` predicates (each with an ASCII fast path).
- The regex engine's class items, named rules, word-boundary tests, `<ws>` and prefilter first
  sets call them. `\d` no longer counts as an ASCII-only class for the first-set prefilter.

`t/regex/regex-cclass-parity.t` checks each class against `nqp::iscclass` over a codepoint sample,
plus 15 rakudo-measured rows. The remaining differences in the probe come from the Unicode data
version (U+088F, U+0C5C, U+0CDC and U+0295 are newer than mutsu's tables).

### 2.7 Directory listing and file tests

Rakudo's `sub dir` is `$path.IO.dir(|c)`, and `$path ~~ :r` is `$path.r`. mutsu had two copies
of the listing and three of the file tests:

- The `IO::Path.dir` method returned a `List` where the sub returned a `Seq`. Both died with an
  `X::AdHoc` where rakudo throws `X::IO::Dir`.
- The two smartmatch copies (VM and interpreter) tested mode bits instead of `access(2)`, and
  tested `rw`/`rwx`/`s`/`z`/`l` against the path string without resolving it against the
  IO::Path's `CWD`.

The fix:

- `Interpreter::dir_listing` is the one listing body, and it throws rakudo's `X::IO::Dir`.
- `native_io::io_file_test` is the one file-test body. `None` means the path is missing: the
  methods turn that into their Failure, and the smartmatch into False.
- `io_exception_error` now stores its message on the exception object, so a caught IO error's
  `.message` is no longer empty.

`t/io/dir-and-file-test-one-body.t` pins 17 rows.

### 2.6 String operator forms and Blob stringification

- `leg` stringified with the pure renderer instead of the comparators' operand coercion, so a user
  `Str` was ignored and a junction did not autothread. It now shares `coerce_str_compare_operands`
  and the junction threading with `eq` / `lt` (`Interpreter::str_leg`).
- The routine forms `&infix:<eq ne lt gt le ge leg ~ ...>` fell back to the pure reduction table,
  which only renders an object's `.gist`. They now coerce their operands as the opcode does, so
  `.sort(&infix:<leg>)` and `cmp-ok` agree with the operator.
- `X::Buf::AsStr` had five hand-written throw sites and no `.message`. `buf_as_str_error` is now the
  one constructor, and it sets rakudo's `object` / `method` attributes. The exception-message table
  renders rakudo's text. `concat_operand_stringy` is the one Blob-to-string rule (only `utf8`
  decodes), shared by `~`, prefix `~` and interpolation. Before, those three spliced in
  lossy-decoded bytes or the gist.

`t/types/string/str-operator-forms-parity.t` pins 29 rows. `cmp` with a user-`Str` object is left as it
is (rakudo compares `.Stringy` there too), because `cmp` has its own structural candidates.

## 3. Consequences

- The three panics and the ten divergences in §1 are fixed.
- The audit that led here found the same pattern in other families, each to be unified the same
  way under this decision: `NqpPure` is re-implemented by TRIR and the JIT; the `nqp::` list
  ops, `AT-POS` and `[]` each resolve indices differently; `nqp::istrue` / `isconcrete` /
  `istype` / `eqaddr` / `clone` bypass the VM's own routines; there are four byte decoders; the
  regex engine's `\d` / `\w` / `<alpha>` ignore the `CCLASS` table `nqp::iscclass` uses.
