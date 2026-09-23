# `bench-regex-capture` regression: a class-table scan on every Match method call

On 2026-09-23 the deterministic instruction count of `bench-regex-capture`
jumped from 1.35G to 2.46G (+82%) at `b07f4758` ("fix: support
Grammar::PrettyErrors parsing"). The same commit also moved the grammar
benchmarks, but those were already recovered by `442a5fef`, which made the
regex engine's token wrap-chain probe bail when no method wrap exists.
`bench-regex-capture` stayed at 2.46G.

## Root cause

`b07f4758` replaced `has_user_method` with a new `grammar_has_user_method` on
the instance and package method-dispatch paths, so that a public method from an
un-punned role composed onto a grammar can override the native
`parse`/`subparse`/`parsefile`. The helper evaluated both halves
unconditionally:

```rust
let direct = self.has_user_method(name, method_name);
let role = self.class_is_grammar(name) && /* role probe over the MRO */;
direct || role
```

`class_is_grammar` walks the parent chain through `resolved_class_parents`.
For a built-in receiver such as `Match` there is no class-table entry, so that
lookup falls back to comparing the name against the `::`-tail of *every*
registered class (a `rsplit_once` substring search each). The benchmark calls
`.Str`, `.chars`, `.from`, `.elems` ... on Match objects 9,600 times; each call
paid ~115k instructions for the scan, 1.1G in total, 44.6% of the run
(callgrind: `StrSearcher::new` alone was 15.7% self time).

## Fix

The three conditions are pure reads combined with `&&`, so they are now
ordered cheapest first: return on a direct user method, then require some
registered role to declare a public method of that name (a hash probe per
role, skipped outright when no role exists), then a role on the receiver's MRO,
and only then the grammar ancestry walk. The answer is unchanged; the
ancestry walk now runs only for a receiver that really has a role-provided
candidate.

`bench-regex-capture`: 2,468,674,050 -> 1,371,013,508 instructions (-44.5%,
callgrind on the profiling build), back to the pre-regression level. The
grammar and YAML benchmarks are unchanged (warm runs).
