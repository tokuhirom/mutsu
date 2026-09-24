# ADR-0117: `Str` methods, `nqp::` string ops, VM string opcodes and TRIR string ops share one routine per primitive

- **Status**: Accepted (2026-09-23, user decision: "String のメソッドや VM op は、nqp:: と原則として
  ルーチンを共通化すべき" -- Str methods and VM ops should in principle share their routines with
  `nqp::`).
- **Deciders**: tokuhirom, Claude
- **Context**: [#9129](https://github.com/tokuhirom/mutsu/issues/9129) (the single-slot
  `nqp_char_cache` cliff), [#9140](https://github.com/tokuhirom/mutsu/issues/9140) (the cached
  grapheme index), [#9147](https://github.com/tokuhirom/mutsu/issues/9147).

## 1. Context

In rakudo a `Str` method is a thin wrapper over a MoarVM string op: `.chars` is `nqp::chars`,
`.substr` is `nqp::substr`, `.index(:i)` is `nqp::indexic`, `.starts-with` / `.substr-eq` /
`.ends-with` are `nqp::eqat` / `nqp::eqatic`, `.flip` is `nqp::flip`, infix `x` is `nqp::x`, `~` is
`nqp::concat`. There is one implementation per primitive, so a method and its op cannot disagree.

mutsu grew **three** independent implementations of the positional string primitives:

| layer | unit | index memo |
|---|---|---|
| `Str` methods (`runtime/methods_string_*.rs`, `builtins/methods_*`) | grapheme | `grapheme_index` (8-slot, per payload) |
| `nqp::` ops (`runtime/nqp_ops_str.rs`, `nqp_ops_text.rs`, `nqp_ops_builtin.rs`) | **codepoint** | `nqp_char_cache` (1 slot, `Vec<char>`) |
| TRIR typed ops (`trir/exec_str.rs`) | **codepoint** | `TrCharCache` (4 slots, `Vec<char>`) |

plus separate copies of the case/mark folding behind every `:i`/`:m` adverb (five of them, all
`to_lowercase`), and four copies of string repetition (the `x` opcode, two reduction paths and
`nqp::x`).

They drifted, and every drift was a divergence from rakudo, measured on
`"ae\x[301]x\r\nY\x[1F1EF]\x[1F1F5]z"`:

| | rakudo | mutsu before |
|---|---:|---:|
| `nqp::chars` | 7 | 9 |
| `nqp::index($s, "Y")` | 4 | 5 |
| `nqp::rindex($s, "x")` | 2 | 2 (`rindex($s, "")`: 7 vs 9) |
| `nqp::ordat($s, 5)` | 127471 | 89 |
| `nqp::eqat($s, "x\r", 2)` | 0 | 1 |
| `nqp::flip` | grapheme-reversed | codepoint-reversed (`\n\r`) |
| `nqp::substr("abcdef", -2, 1)` | `e` | `ab` |
| `"STRASSE".starts-with("straße", :i)` | True | False |
| `"straße".index("SS", :i)` | 4 | Nil |

The codepoint layers also carried the O(n^2) cliff of #9129: a loop touching two strings
alternately missed the single-slot memo on every call.

## 2. Decision

1. **`src/builtins/str_prim/` is the single home of each `Str` primitive**: `chars`, `slice`,
   `nqp_substr`, `index` / `rindex` (+ the `nqp_*` edge-case wrappers), `eq_at`, `affix_matches`,
   `contains`, `char_at` / `nqp_ordat`, `find_char` (cclass scans), `graphemes`, `flip`, `concat`,
   `repeat`, `normalize`, and `Fold` (exact / `:i` / `:m` / `:i :m`).
2. **Positions are graphemes everywhere**, resolved through the cached `GraphemeIndex`, because
   MoarVM strings are NFG. The `nqp::` layer stops being codepoint-indexed.
3. **`:i` is the Unicode full case fold (`fc`), `:m` drops combining marks, per grapheme**, and a
   folded hit must start and end on a folded-grapheme boundary -- `nqp::indexic`'s rule. The `Str`
   adverbs use the same `Fold`, so `.index(:i)` and `nqp::indexic` are literally one call.
4. **Callers differ only in how they report an edge case**: a `Str` method returns `Nil` or a
   `Failure`, the `nqp::` op returns `-1` / dies with MoarVM's message, the TRIR op returns a
   native int. The search, the slicing and the folding are shared.
5. **No layer keeps a private string memo.** `nqp_char_cache.rs` and `TrCharCache` are deleted;
   the `grapheme_index` payload cache serves every layer.
6. **Enforced, not requested**: `scripts/check-str-prims.sh` (`make check-str-prims`, since
   generalized to `scripts/check-prims.sh` / `make check-prims` by ADR-0118; a
   `make test` prerequisite and a CI step) fails the build when an `nqp::` op table, the VM's nqp
   path or TRIR's runtime walks, cases, normalizes, repeats or searches a string by hand, and
   bans the old memo names anywhere in `src/`. A line that is genuinely not a Str primitive
   (parsing digits, lowercasing an encoding name) opts out with a `str-prim: allow` comment.
   `t/vm/nqp-str-prim-parity.t` pins the behaviour: every row checks the rakudo-measured value
   and that the op and the method agree.

## 3. Consequences

- The nine divergences in the §1 table are fixed, and `nqp::substr`/`index`/`rindex`/`eqat` now
  follow MoarVM's argument rules (negative start from the end, `rindex` past the end dies).
- #9129's two-string cliff is gone: the grapheme index cache is keyed per payload with eight
  slots, and a flat ASCII string needs no index at all.
- On a non-flat (non-ASCII or `\r\n`) string a position costs O(STRIDE) (32 graphemes of
  segmentation from the nearest checkpoint) rather than O(1) from a `Vec<char>`. That is the price
  of being grapheme-correct; a scanner over flat ASCII text, the common case, pays nothing.
- `Str`-level code outside the three delegating layers is not policed by the script (it has many
  legitimate char-level operations). New positional `Str` methods should still be written on
  `str_prim`, and a reviewer should treat a new private search/fold there as a violation of this
  ADR.
