# Multi dispatch now honours a named parameter's aliases

A named parameter may carry aliases: `:s(:$sort)` answers to both `:s(…)` and
`:sort(…)`. Binding already honoured both spellings, but multi-candidate
*matching* only looked at the first name, so

```raku
multi f(Int $n, :s(:$sort) = False) { }
f(1, :sort(True));   # raku: fine   mutsu: No matching candidates for proto sub: f
```

failed — while the identical signature on a plain `sub` accepted it. That
asymmetry is the tell: the alias was known to the binder and invisible to the
dispatcher.

## Root cause

The parser records aliases as nested *named* entries in the parameter's
`sub_signature`: `:s(:$sort)` becomes
`ParamDef { name: "s", named: true, sub_signature: [ParamDef { name: "sort",
named: true }] }`. `types/signature.rs` walked that nesting when collecting
consumed named keys; `types/args_matching.rs` did not — both its
"is this named argument consumed by any parameter?" check and its
required/`:D`/`where` lookup compared only the outer `pd.name`.

Both now go through a new `ParamDef::named_external_keys()`, which returns every
external key a named parameter answers to (sigil- and twigil-stripped, plus each
alias). Candidate selection is unchanged otherwise: an unknown named argument is
still rejected, and the positional types still pick the candidate.

## Found by

The `--run-tests` axis of the real-dist compatibility sweep (PLAN §B4), which runs
each loading dist's own suite with raku as the baseline. `Prime::Factor` graded
`test_die`: its `multi divisors (Str $n, :s(:$sort) = False)` re-dispatches with
`samewith (+$n).narrow, :sort($sort)`, and that inner call had no matching
candidate, so its test files aborted at 84 of 87. All four of its files now match
raku exactly (295 subtests, 0 failures).

That sweep axis is worth repeating: of the 28 dists that load, only 7 pass their
own suite, 5 fail and 14 die — a much sharper frontier than the load-only view.
The remaining ones are separate root causes, recorded in
`todo/tickets/dist-test-suite-failures-batch.md`.

Pinned by `t/multi-aliased-named-param.t` (12 subtests: long and short spellings
on a multi, the plain-`sub` control, a required aliased named via either name, an
unknown named still rejected, a type constraint, a `where` constraint, a multi
method, and candidate selection across two positional types). All 12 identical
under raku.
