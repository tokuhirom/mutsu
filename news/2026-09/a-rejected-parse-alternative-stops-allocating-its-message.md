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

## An unexpected finding: a fixed 668M cost in the first `from-json` call

This was expected to pay off at **module-load** time. It does not. A load-only
script (`use JSON::Fast` and nothing else) is unchanged by it: **65,913,860**
instructions before, **66,074,346** after. The whole 76.8M saving lands in the
run that calls `from-json`.

Chasing that produced a larger finding, and one limit on it. What is measured:

- Adding a single `from-json('[1]')` — a **three-character** document — to the
  load-only script takes it from 65,901,767 to **733,759,960** instructions.
  That is **668M for parsing three characters**, so it is a fixed cost of the
  first call, not per-record work.
- `mutsu::parser::*` accounts for **152,065,101** of it, against **31,624** in
  the load-only run. So the Raku parser is doing real work triggered by that
  call.
- It is **not** a precompilation cache miss: a second run of the same script is
  735,201,391, and `touch`ing the load-only script leaves it at 65,912,976.
- It is **not** a pathological parse of the call expression:
  `--dump-ast -e "my \$d = from-json('[1]');"` is 1,663,027 instructions, and
  the same line with a plain identifier (`f`), without the hyphen, or with a
  non-string argument are all within 0.3% of that.

**What is not established is the cause**, and it is worth recording what has
been ruled out so the next attempt does not repeat it. Under `rust-gdb`,
`mutsu::parser::parse_program` is entered exactly **once** (the script itself)
and `parse_dispatch::parse_source` **once** (the `Enumeration` role prelude,
which the load-only script triggers too, so it is not the difference);
`parse_program_with_operators_and_user_subs` and
`parse_program_partial_with_operators` are never entered at all. JSON::Fast
declares no regex, so runtime regex compilation is not it either. The 152M is
reached through an entry point I did not identify. It wants its own issue and a
fresh look, not a guess.

Two consequences that do follow from the measurements alone:

1. **It explains this slice's wall clock.** What this change removes is
   concentrated in that fixed cost, so it is 3% of a 100-record run and
   roughly a seventh of that at 727 records — and a 0.4% difference is not
   something seven runs on this box can see.
2. **The 100-record reproduction over-weights fixed cost.** 668M of its
   2,472M — **27%** — is the first `from-json` call regardless of document
   size. A share of *every* percentage measured on it, including the ones in
   #8830's own body and in the three earlier slices, is that fixed cost rather
   than per-record work. Measure at 727 records, or subtract a load-only run
   first.

Pinned by the existing parser tests — `merge_expected_messages`'s two unit
tests in `parser/stmt/tests_3.rs` cover the merge directly, and the parse-error
messages themselves are asserted throughout `t/` and roast, so a `Cow` that
borrowed the wrong thing would be a compile error and a wrong message would
fail loudly.
