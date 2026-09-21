# `package_type_alias` skips `has_type_direct` when nothing aliases the name

[#8899](https://github.com/tokuhirom/mutsu/issues/8899) measured run-time name resolution at
13.6% of a `JSON::Fast` decode that resolves no names dynamically. One named contributor was the
type-name resolution cluster: `has_type_direct`, `package_type_alias`,
`try_resolved_type_capture_name` and friends, re-resolving a declaration's type constraint on
every execution even though the constraint string is fixed at compile time.

`package_type_alias(name)` unconditionally called `has_type_direct(name)` first — four
`HashMap::contains_key` probes against the class/role/enum/subset tables — before even checking
whether `name` appears in any package's alias table at all. For a program that aliases nothing
under a given name (every builtin type constraint — `Str`, `int`, `Bool`, ... — in a module that
imports no colliding short name), that `has_type_direct` call was pure waste: the subsequent alias
lookup was always going to answer `None` regardless of what `has_type_direct` said.

`lookup_in_running_package` already has its own cheap short-circuit
(`PackageKeyed::contains_name`, one `HashSet` lookup against a memoized union of every package's
alias keys) for exactly this "does this name appear anywhere in the table" question. Reordering
`package_type_alias` to try that first — and only pay for `has_type_direct` when the name really
does have a candidate alias — preserves the exact same observable result (both checks are pure
reads of registry state, so their relative order cannot change the answer) while skipping the
four-probe cost on the overwhelmingly common miss.

Measured (callgrind, `tmp/jf-decode.raku`, 10 decodes of a 100-record document, `--profile
profiling`, re-baselined from `main`): calls into `has_type_direct` from `package_type_alias`
dropped from 1,177,061 to 0 per run; total calls to `has_type_direct` across all callers dropped
78.4% (1,500,276 → 323,215); `has_type_direct`'s own inclusive instruction cost dropped 78.4%
(109.4M → 23.6M Ir), `package_type_alias`'s dropped 5.3% (110.6M → 104.8M Ir) — about 91.7M
instructions per 10 decodes (~9.2M per decode) attributable directly to this change.

This is one bounded slice of #8899's cluster, not the whole finding — the issue stays open per
the perf-tuning skill's guidance ("do not close a `todo:perf` issue from the PR that lands one
slice of it").
