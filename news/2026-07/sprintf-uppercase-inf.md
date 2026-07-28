# Uppercase infinity for `%G`, and a language revision that survives a nested parse

Under `use v6.e.PREVIEW`, `sprintf` now renders infinity and NaN as `INF` /
`-INF` / `NAN` for the uppercase `%G` directive; `%g` keeps producing `Inf`,
`-Inf` and `NaN`, and so does `%G` under 6.d and earlier. The casing is a
6.e-only change, so it is gated on the same `v6e_active()` check the other 6.e
sprintf flag semantics already use — `roast/6.d/S32-str/sprintf.t` asserts the
old casing and `roast/S32-str/sprintf.t` (a `use v6.e.PREVIEW` file) asserts the
new one, and both now pass.

The blocker ledger had attributed the remaining failures to zero-padded
scientific `%g` formatting. Re-measurement showed that those cases already
passed in both mutsu and Rakudo, so the stale diagnosis was removed.

Gating on the language revision surfaced a much broader bug: the revision did
not survive a nested parse. `use vX` is recorded in a parser thread-local, and
`parse_program` / `parse_program_partial` reset that global to the 6.d default at
parse start. Every nested parse the runtime performs therefore silently
downgraded the enclosing compilation unit:

- the module export scan (`extract_module_exported_operator_names`), which runs
  *before* `load_module` takes its own snapshot, so any `use SomeModule` dropped
  the importing program to 6.d;
- `EVAL`, so a single `EVAL('sprintf("%b",1)')` on line 17 of a
  `use v6.e.PREVIEW` file changed how the remaining 150 lines compiled;
- the embedded `{...}` blocks in a regex, the injected preludes, the `.AST`
  round-trip, and `CompUnit::Repository.need`.

Because the revision gates far more than sprintf — submethod dispatch, grammar
`.parse` returning a `Failure`, subset `Nil` nominalization — this was a general
correctness hole that happened to be cheapest to observe through sprintf.

Nested parses now restore the caller's revision. `parse_dispatch::parse_source`
is the nested-safe default and a new `parse_compilation_unit` is used by the
three call sites whose statements then run under the parsed unit's own pragma
(the main program, a `use`d module, a `require`d file); `load_module` still
restores the importer's revision once the module mainline has finished, and the
two `CompUnit` load paths now do the same. `EVAL` additionally *inherits* the
caller's revision rather than starting at the 6.d default, matching Rakudo:
`use v6.e.PREVIEW; EVAL 'sprintf("%#x", -256)'` is `-0x100`.

Pinned by `t/language-version-not-leaked-by-nested-parse.t` and the new cases in
`t/sprintf-6e.t`.
