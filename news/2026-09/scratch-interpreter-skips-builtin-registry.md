# Regex scratch interpreters no longer build the built-in registry

Every regex/grammar sub-evaluation — a subrule called with arguments, an
embedded `{ ... }` block, a `<?{ ... }>` assertion, a `.^lookup`-style token
method — builds a scratch `Interpreter` so the evaluation cannot mutate the
caller's environment. `Interpreter::new` built the entire built-in declaration
registry for each one: ~450 `ClassDef`s, the `X::` exception hierarchy, the
composed-role seeds, and a freshly seeded method table.

That work was pure waste. Every one of the ten scratch construction sites
immediately replaces the scratch's registry with the caller's
(`copy_decl_registry_into` / `copy_full_registry_into`) before the scratch runs
anything. `benchmarks/bench-yaml-parse.raku` builds **207** scratch
interpreters per parse, and a callgrind profile attributed **~48% of the whole
run** to those constructions (`Interpreter::new` reached from
`eval_regex_expr_value`, `resolve_token_patterns_with_args_in_pkg`,
`regex_match_atom_in_pkg_inner`, ...).

## The fix

`copy_decl_registry_into` used to clone four maps (`functions`,
`proto_functions`, `token_defs`, `enum_types`) into the registry the scratch
had built for itself, leaving it on its own built-in `classes`/`method_entries`.
It now shares the parent's copy-on-write `Arc<Registry>` — what
`copy_full_registry_into` already did for the actions path. That is O(1)
instead of four map clones plus a registry write, and it is a strict superset
of the data: the parent's registry carries every built-in the scratch used to
build for itself, plus the user declarations it previously could not see.

With no caller reading the scratch's self-built registry, `Interpreter::new`
skips building it under the existing `BUILDING_SCRATCH` flag (which already
skipped the `%*ENV` sweep, IO handles and `$*REPO` setup for the same reason).
The construction moved verbatim into `Interpreter::build_builtin_registry`,
called only for a real interpreter.

## Effect

Release build, idle box, median of 7 runs:

| workload | before | after | raku (same box) |
| --- | ---: | ---: | ---: |
| `benchmarks/bench-yaml-parse.raku` | 0.344s | **0.138s** | 0.340s |
| a 120-row YAML document | 2.97s | **0.96s** | 0.44s |

The `t/` suite's total CPU time drops ~11% (352s → 312s), since every
grammar-using test pays this. `registry-cow: clones` stays 0 on the benchmark,
so the shared registry is not trading construction for copy-on-write clones.

(The `before` column is already after the same session's aliased-capture action
fix — see `news/2026-09/grammar-alias-action-exponential-firing.md`. Against
this ticket's session-start baseline of 1.14s, `bench-yaml-parse` is 8.3x
faster and now runs 2.5x faster than rakudo on the same box.)
