# A `Test` assertion's intern count is exact again, and a cached module's `my class` keeps its identity

`tests/named_call_intern_budget.rs` measures how many `Symbol::intern` calls one `Test` assertion
makes. It is meant to be exact, but repeated runs of one binary gave 11.98 to 12.02 against a
budget of 12.0, so `make test` passed or failed at random (#9733).

The variation had two sources.

- `import_module` interned `GLOBAL::<name>` once for each `proto_functions` entry it scanned. That
  table grows in hash order while the import runs, so the count changed from run to run. This was
  already fixed on `main` by hoisting the intern.
- The precompilation cache did not serialize the `decl_id` of a `ClassDecl`. A cached node came
  back with id 0, and id 0 turns off ADR-0047's lexical storage name. So `Test.rakumod`'s
  `my class X::SubtestsSkipped` was registered as `X::SubtestsSkipped\0<id>` when the module was
  parsed, and under the bare name on every cache hit. Whether a load was cold or warm therefore
  changed the registry, and it also changed which interns the import made. This is the
  warm/cold divergence that `src/precomp.rs`'s module docs warn about. A deserialized
  `ClassDecl` now gets a fresh id, exactly as a re-parse would.

Two per-assertion interns are also gone. The `IO::Handle` output fast path now checks for a user
`say` with the method `Symbol` it already holds. The type-alias walk now skips composite
spellings: a coercion type such as `Bool(Mu)` or a parameterization such as `Array[Int]` can never
be an alias's key.

The assertion now costs exactly 9.000 interns on every run, cold or warm cache. The budget is now
10.0.
