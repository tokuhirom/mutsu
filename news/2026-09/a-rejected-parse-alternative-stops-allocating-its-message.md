# A rejected parse alternative stops allocating its message

Fourth slice of [#8830](https://github.com/tokuhirom/mutsu/issues/8830). With
the first three gone, the top of the `JSON::Fast.from-json` profile is the
allocator, and its single largest identified source was not the interpreter at
all — it was the **parser's error bookkeeping**, at 525,110 allocations, **28%
of every allocation the process made**.

`PError::messages` was a `Vec<String>`, and `PError::expected(what: &str)` did
`vec![what.to_string()]`. Of the 442 construction sites, **all but eight pass a
string literal** — `PError::expected("closing paren")`,
`PError::expected("statement end")`. Every one of them copied a `&'static str`
onto the heap.

A backtracking parser constructs a failure for every *rejected* alternative, so
almost all of those errors are discarded microseconds later by an alternative
that did match. This is the one place in the codebase where the cost of
describing an error is paid on the **success** path, and it was being paid in
heap allocations.

`messages` is now `Vec<Cow<'static, str>>`, and `expected` / `expected_at` /
`raw` take `impl Into<Cow<'static, str>>`. A literal becomes `Cow::Borrowed`
and allocates nothing; the eight dynamic sites pass an owned `format!(...)`
(dropping a stray `&`) and are unchanged in behaviour.

`merge_expected_messages` came along for the same reason. Its `context` is a
literal at all but three call sites, and a subslice of a `'static` str is
itself `'static`, so the prefix strip and the trim keep borrowing rather than
copying. Only a `format!`-built context can allocate there now.

## Measurements

`callgrind`, 100-record parse, `--profile profiling` build, against `main` at
`9414ef50`.

| | before | after | |
| --- | ---: | ---: | ---: |
| **program total** | 2,548,759,824 | 2,471,953,037 | **-3.01%** |
| **allocations, whole program** | 1,900,890 | 1,638,337 | **-13.8%** |
| ... from `PError::expected`/`_at` | 525,110 | **0** | -100% |
| `PError::expected` (self) | 13,583,350 | 276,328 | **-98.0%** |
| `malloc` | 165,852,198 | 140,461,751 | -15.3% |
| `free` | 197,137,199 | 169,948,429 | -13.8% |
| `_int_free` | 123,569,678 | 106,620,482 | -13.7% |
| `mutsu::parser::*` (self, summed) | 199,084,427 | 183,924,447 | -7.6% |

**Wall clock at 727 records did not move**: 1.918s before, 1.929s after, seven
runs each, distributions overlapping. That is not a contradiction — see the
next section. The saving is concentrated in work that happens *once*, so a
document seven times larger dilutes it by roughly seven, well under this box's
run-to-run spread. The deterministic counts above are the evidence.

## An unexpected finding: the parser runs *inside* `from-json`

The change was expected to pay off at module-load time. It does not — a
load-only script (`use JSON::Fast` and nothing else) is unchanged by it,
65,913,860 instructions before and 66,074,346 after. The whole 76.8M saving
lands in the `from-json` call itself.

So mutsu's **Raku parser is running during a JSON parse**, to the tune of
roughly 120M instructions — `mutsu::parser::*` is 183.9M in the benchmark
against at most 66M for the entire load-only run. `keyword_literal` alone is
entered 43,814 times and `parse_prefixed_radix` 11,087.

The likely cause is one of the lazily-parsed preludes in
`runtime/run_prelude.rs` (the Rational role is the obvious suspect for a
document full of numbers) being parsed on first use, which happens to fall
inside the timed region. That would make it a one-off cost a larger document
amortizes — but it is ~5% of this 100-record profile, and it means **a share
of every percentage measured on the 100-record reproduction is a fixed
startup-shaped cost, not per-record work**. Worth confirming and filing
separately; it is not this slice's to fix.

It also explains this slice's own wall clock. If the parser cost is a one-off,
then so is most of what this change removes: at 100 records it is 3% of the
run, at 727 records roughly a seventh of that, and a 0.4% difference is not
something seven runs on this box can see. The consequence for whoever takes
the next slice is that **the 100-record reproduction over-weights startup** —
either measure at 727 records, or subtract a load-only run first.

Pinned by the existing parser tests — `merge_expected_messages`'s two unit
tests in `parser/stmt/tests_3.rs` cover the merge directly, and the parse-error
messages themselves are asserted throughout `t/` and roast, so a `Cow` that
borrowed the wrong thing would be a compile error and a wrong message would
fail loudly.
