# Regex and grammar code runs on the caller's interpreter

The regex engine no longer builds a scratch `Interpreter` when it has to run
Raku code in the middle of a match (#10151). Eleven sites used to construct
one — `<{ … }>` interpolation, `** { … }` quantifiers, subrule argument
expressions, parameterized `token t($x)` bodies, regex values called with
arguments, `:my` declarations that need an env diff, grammar methods called as
subrules, custom-HOW `find_method` dispatch, the no-capture length probe, and
the in-parse action runs behind `$<x>.made` and the reduce-time `$*` publisher.
Each paid an interpreter construction plus a registry copy per call.

They now all go through `Interpreter::run_regex_sub_eval`
(`src/runtime/regex/regex_sub_eval.rs`), which runs the code on `self` and
keeps the one property the scratch provided, env isolation, by swapping the
copy-on-write `Env` out and back (O(1) both ways) together with the
compiled-local writeback log. Everything else is the caller's own, so what the
scratch could only approximate by copying — the registry, the IO handle table,
the unit/package lexical stores — is simply shared, and state the scratch used
to drop is kept: a `state` variable bumped by a routine called from `<{ … }>`
or `** { … }` now keeps its value, as in Rakudo
(`t/regex/regex-code-runs-on-caller.t`).

`new_regex_scratch`, `new_regex_scratch_sharing_io` and the `BUILDING_SCRATCH`
shortcuts in `Interpreter::new` are gone. The `check-interp-construction`
ratchet now also counts the `Interpreter { .., ..Default::default() }`
spelling, which exposed one more site, the regex parse-time
`eval_string_as_source`; it is listed as debt under #10157.
