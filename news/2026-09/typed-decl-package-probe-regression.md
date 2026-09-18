# Two hot paths that started resolving a name on every execution

The deterministic instruction-count series stepped at one main commit
(`7fda0c86`, 2026-09-18): `bench-json-fast` went 3,348M -> 6,905M simulated
instructions (+106%), `bench-yaml-parse-big` +36%, `bench-grammar-parse-big`
+33%, `bench-grammar-parse` +9.8%, `bench-grammar-parse-deep` +7.1%,
`bench-yaml-parse` +8.5%. Everything else — fib, the arithmetic rows, class,
hash, method-call — moved less than 0.2%, and the toolchain column read
`rustc 1.96.0` on both sides, so this was not the legible whole-series step a
compiler bump produces. It was two unrelated regressions from the same commit,
each one a name resolution that moved onto a per-execution path.

Both came from the Dan ecosystem fix, and neither was visible in the wall-clock
series: its noise floor is 16-33% between consecutive main commits, which
swallows a 33% step whole. This is exactly the case the instruction-count
series was added for (#8085).

## The typed declaration probe

`exec_set_var_type` — the op behind `my Int $x` and every typed container form
— gained a package-chain resolution for any unqualified constraint, so that a
user type used unqualified inside a module keeps the package-qualified identity
the type checker uses (`Array[Dan::DataSlice]`, not `Array[DataSlice]`). That
is correct and is kept.

What made it expensive is that the op runs on every *execution* of the
declaration, not once per declaration, and that the walk it performs ends in
`resolve_lexical_type_key`, whose miss path is a linear scan of every key in
the class, role, enum and subset registries (`key.starts_with("{qualified}\0")`).
JSON::Fast declares 43 native-typed locals (`my int`, `my str`, `my uint32`)
inside its hot loops. Measured on bench-json-fast with callgrind:

| | before | after |
| --- | --- | --- |
| `exec_set_var_type` (inclusive) | 264M (7.3%) | 3,813M (53.0%) |
| `resolve_type_in_current_package` calls | 8,298 | 183,666 |
| `resolve_lexical_type_key` (exclusive) | 71M | 1,598M |
| `__memcmp_avx2_movbe` | 140M (3.9%) | 1,604M (22.3%) |

A core type name cannot be the thing that fix was about, and a package-local
declaration does not normally shadow one, so the probe is now skipped for a
core name that nothing shadows. That test already existed inline in
`resolve_type_name_for_owner`, which grew it for the same reason (it was
allocating an `Owner::Int` candidate for every ordinary attribute on every
`.new`); it is now a named helper, `unshadowed_builtin_type_name`, used by
both. The skip is conditional, not a blanket "core names never resolve": a
`subset Str of Int` declared inside a package really does shadow the builtin,
and still resolves qualified.

## The subrule lexical probe

`may_name_lexical_regex` decides whether a subrule reference could name a
lexical `Regex` rather than a registry rule. It had been a pure string test —
`<&x>` or `<&$x>` — precisely so that the overwhelmingly common `<rule>`
reference pays nothing. It was widened to accept every unqualified name, on the
grounds that `my regex name { ... }` is callable as `<name>` and lives in the
lexical `&name` lane.

That made every ordinary `<subrule>` call in every grammar run the lexical
fallback: a `format!("&{bare}")` allocation and an env walk, per reference, per
match attempt. On bench-grammar-parse, `resolve_lexical_regex` went from 24,726
to 90,774 instructions and dragged the formatting machinery with it
(`format_inner` 52K -> 269K, `core::fmt::write` 43K -> 209K, `Env::get_sym`
7K -> 102K, `Env::base_get` 3.6K -> 79K).

The widening turns out to be unnecessary: the same commit added a parent-scope
walk to `resolve_token_patterns_static_in_pkg` and
`collect_token_defs_for_scope_dedup`, and that is what actually makes a
`my regex` declared in a module visible to a nested role's method. With the
cheap predicate restored, `t/modules/package-lexical-subrule.t` — the test that
commit added for exactly this — still passes, as do the file-scope, block-scope
and grammar-token spellings, all checked against `raku`.

## What this says about the two series

The wall-clock history would not have caught either of these, and did not: its
own regression alarm fires at 1.5x, inside the p90 of its own noise. The
instruction-count history named the commit on the first look and the callgrind
function profile named the two call sites without a bisect. Read them together.
