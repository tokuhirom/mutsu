# A when-only block's non-match value is still wrong outside map/grep/first

**Reclassified from `todo/tickets/` (2026-08-18):** the ticket's own analysis
already concluded a point fix isn't safe — the general fix means making
`exec_when_op` push a value onto the VM stack on every non-match, which is a
statement-sequencing invariant change with three separate call sites
(`compile_unit`, `compile_block_inline`, every loop-body compiler) that must
be updated in lockstep or the interpreter silently accumulates or loses stack
values across unrelated statements — a correctness-critical, hard-to-detect
class of bug if any one site is missed. All three repro probes below were
re-confirmed reproducing on `main` as of 2026-08-18 (unchanged from the
original write-up). Filing here rather than attempting a narrow patch.

`todo/tickets/when-only-block-nonmatch-value-wrong.md` fixed the value a
`when`/`default`-tail block evaluates to when nothing matches, for the four
inline `.map`/`.grep`/`.first` fast paths. The same wrong-value disease
exists in every OTHER context a block or `given`/`when` can produce a value
in, through different fallback paths:

```raku
my $b = { when 2 { "two" } }; say $b(3).raku;      # raku: Bool::False   mutsu: Nil
$_ = False; my $a = do { when .so { "foo" } };     # raku: Bool::False   mutsu: Any
say (given 3 { when 2 { "two" } }).raku;            # raku: Bool::False   mutsu: Nil
```

## Root cause

`exec_when_op` (`src/vm/vm_given_when_ops.rs`) now records the correct falsy
value in `Interpreter::when_nonmatch_value` on a non-match (see the fix
above), but nothing pushes it onto the VM stack — the four map/grep/first
sites consume the field directly instead. Every other caller of a
when-tail block (a direct closure call, `do { when ... }`, a bare
`given`/`when` statement) relies on the block's own compiled return value,
which is `Nil`/`Any` because nothing was pushed.

## The general fix (from the original ticket's "Follow-up" section)

Make `exec_when_op` push the falsy value on the stack on non-match — the
architecturally correct end state, but with a real blast radius:

- `compile_unit` (`src/compiler/mod.rs`, ~line 2679) already emits a `Pop`
  after a non-last `when` — currently a no-op since nothing was pushed; it
  would become load-bearing.
- `compile_block_inline` does NOT pop non-last whens and appends a trailing
  `LoadNil` after a tail when
  (`src/compiler/helpers_block_inline.rs`, ~line 387) — that would bury the
  newly-pushed value and needs suppressing for a when-tail block.
- Loop bodies (`for`, `while`, ...) would accumulate one value per
  non-matching iteration unless every loop op explicitly truncates the
  stack after each pass.

This needs a careful stack-hygiene sweep across all three areas, not a
point fix — treat as a dedicated session. The `when_nonmatch_value` marker
already added is a stepping stone this can reuse (or replace, once the
stack itself carries the value).

## Verification

Probe rows 7, 31, 32 from the original ticket's investigation (all
re-verified against Rakudo v2026.06):

| # | Probe | raku | mutsu (current) |
|---|---|---|---|
| 7 | `my $b = { when 2 { "two" } }; say $b(3).raku` | `Bool::False` | `Nil` |
| 31 | `$_ = False; my $a = do { when .so { "foo" } }; say $a.raku` | `Bool::False` | `Any` |
| 32 | `say (given 3 { when 2 { "two" } }).raku` | `Bool::False` | `Nil` |

Add a `t/` pin for all three once fixed. `t/when-only-block-nonmatch-value.t`
test 11 (currently `todo`-marked) exercises probe row 7 and should flip to a
plain passing assertion.
