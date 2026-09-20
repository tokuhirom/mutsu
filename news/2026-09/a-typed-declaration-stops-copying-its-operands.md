# A typed declaration stops copying its operands

Third slice of [#8830](https://github.com/tokuhirom/mutsu/issues/8830). The
first two removed whole-table walks from name resolution; with those gone the
top of the `JSON::Fast.from-json` profile is the allocator — `malloc`, `free`
and their internals together about 14% of instructions retired, across
2,045,695 allocations to parse 100 flat JSON records.

The single largest identified allocation site was `exec_set_var_type`, the op
behind every `my TYPE $x`, at **172,747 of them**. All four were copies of a
string the function already had.

```rust
let name = Self::const_str(code, name_idx).to_string();
let raw_constraint = Self::const_str(code, tc_idx).to_string();
```

`code` is a `&CompiledCode` parameter, not borrowed from `self`, so both
constant-pool `&str`s can be held as-is for the whole function — the
surrounding `&mut self` calls borrow something else. (`exec_get_bare_word_op`
two files over already does exactly that.) The `.to_string()`s were pure
overhead: two allocations and two frees on every execution of a typed
declaration.

Two more on the same path:

- `resolved_type_capture_name` is `try_resolved_type_capture_name(...)
  .unwrap_or_else(|| constraint.to_string())`, and the `try_` half exists
  precisely so a caller need not pay for the passthrough copy (#8815 split it
  out after the package-alias branch cost bench-array ~16% in `String`
  alloc/free churn). This caller was still taking the allocating form, for a
  constraint like `int` or `Str` that has no capture, generic or alias to
  resolve. It takes a `Cow` now.
- `constraint.contains("::")` and `constraint.contains('[')` built a
  `StrSearcher` and a `CharSearcher` per execution, for a fixed two-byte and
  one-byte needle. `str_scan.rs`'s `has_double_colon` and `has_bracket` are the
  byte scans that already replaced these spellings elsewhere.

## Measurements

`callgrind`, 100-record parse, `--profile profiling` build, against `main` at
`d8937a02`.

| | before | after | |
| --- | ---: | ---: | ---: |
| program total | 2,584,028,104 | 2,547,894,577 | **-1.40%** |
| **allocations, whole program** | 2,045,695 | 1,900,639 | **-7.1%** |
| ... from `exec_set_var_type` | 172,747 | **0** | -100% |
| `free` | 211,241,728 | 197,162,796 | -6.7% |
| `_int_free` | 132,027,142 | 123,605,390 | -6.4% |
| `<&str as Pattern>::is_contained_in` | 11,988,491 | 8,082,615 | -32.6% |

**Wall clock did not move.** Seven runs each on #8673's 727-record
reproduction, release builds of the same tree: 2.008s before, 2.011s after —
the distributions overlap completely and the difference is noise. A percent
and a half of instructions is simply below what this box can resolve; the
deterministic counts above are the evidence, and the honest summary is "this
does strictly less work" rather than "this is faster on a stopwatch". (The
absolute numbers are also lower than the 2.25s the previous slice measured an
hour earlier — the box's own speed drifted between the two sessions, which is
exactly why only paired A/B runs are worth quoting.)

## Why it is still worth landing

The allocator is the largest remaining cluster in this profile, and it is not
a single fixable call site — it is 1.9M allocations spread across the
interpreter. Removing them is a sequence of changes like this one, each
individually small. This one takes out 7% of the total in a single op, with no
new mechanism, no cache to invalidate, and no behaviour change.

The next-largest identified sites, for whoever continues:

- `PError::expected` (297,310) and `PError::expected_at` (227,800), with
  `drop_in_place<PError>` freeing 386,143 — the parser's per-alternative error
  bookkeeping. These are **module-load** cost, not parse cost (a load-only
  script runs in 17ms of the benchmark's 250ms, and `mutsu::parser::*` is 7.69%
  of the profile), so fixing them helps the startup time of every mutsu
  program rather than this benchmark.
- `<String as Clone>::clone` (330,580), of which 91,324 are `current_package()`
  — an `RwLock` read plus a `String` clone, where `current_package_sym()`
  already hands back a `&'static str` for free. There are 198 call sites, so
  which of them is hot needs its own measurement before any sweep.
