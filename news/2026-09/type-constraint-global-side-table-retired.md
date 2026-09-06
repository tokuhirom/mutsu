# The global name-keyed type-constraint side table is gone (ADR-0042 slice 3)

`Interpreter::var_type_constraints` — a `HashMap<String, String>` keyed by BARE
variable name, process-global and never frame-scoped — has been deleted, along
with its twin `var_hash_key_constraints` and the workarounds that existed only
because it was unscoped. This closes
[ADR-0042](../../docs/adr/0042-type-constraints-belong-to-the-container-not-to-a-name.md)
and the deep ticket it addressed
(`todo/deep/bare-name-type-constraint-store-is-scope-blind.md`, which this file
replaces).

A type constraint now lives in exactly two places, both of which travel with the
thing they describe:

- **on the container** — `ContainerCell`'s scalar `of`, `ArrayData`/`HashData`'s
  `value_type` / `key_type` / `declared_type`. This is the only source that can
  answer for a value reached through a *differently-named* bound alias, and it
  is what every enforcement chokepoint reads first.
- **in the env-scoped `__mutsu_type::<name>` / `__mutsu_hash_key_type::<name>`
  entry** — the by-name lane, for the paths that have a name and not yet a
  value. It is dropped with the frame or block that declared it.

## What was measured first

The ticket's own 2026-08-27 note asked for a full re-measurement before any
further slice was dispatched, because five of the rows it and ADR-0042 §3 used
as motivation had started agreeing with `raku`. That re-measurement was done
first, over a 47-row matrix — ADR-0042 §2.1's ten scalar scope shapes, §2.2's
seven container shapes, §3's eight-shape alias probe plus the scalar and
sigilless alias, the five spot-check rows, seven "an outer typed declaration
must keep enforcing after a shadow" rows, and ten rows covering the store's
non-enforcement readers (Nil-to-type-object, `state`, typed parameters, the
object-hash key type, multi-parameter `for` binding).

**All 47 rows already agreed with `raku`.** There was no correctness bug left to
fix; what remained was the architecture. The matrix is now pinned verbatim in
`t/typed-constraint-store-matrix.t`, whose output is byte-identical under
`mutsu` and `raku`, so none of it can silently regress.

## What the deletion removed

- the `var_type_constraints` and `var_hash_key_constraints` fields, their
  initialisation, their per-thread clone, and their clone into the regex-scratch
  interpreter;
- `snapshot_var_type_constraints` / `restore_var_type_constraints`, and with them
  the whole-map snapshot/restore `subtest` wrapped around every one of its runs
  and the copy `throws-like` made into its nested interpreter;
- `throws_like`'s fold of each copied lexical's *effective* constraint into that
  nested map, which existed purely because the map-only read never consulted env;
- `var_type_constraint_fast` and both of its callers — the `GetLocal` and
  `GetGlobal` Nil-to-type-object readers. A typed scalar's type-object seed comes
  from its declaration and its Nil-reset from the store path
  (`typed_scalar_nil_seed_value`), so the Nil that reaches those sites is a
  `= Nil` parameter default and is genuinely Nil;
- `bind_param_type_constraint`'s global CLEAR and
  `set_var_type_constraint_impl`'s — the ticket's "residual 3", an untyped inner
  declaration deleting an outer scope's entry. The caller's entry now lives in
  the caller's env and is restored with it, so this is structurally impossible.

`var_type_constraint` itself is now a single env probe, and
`var_hash_key_constraint` an env probe plus the class-registry fallback an
object-hash *attribute* needs (an attribute is not a lexical, so no by-name
lexical lane can carry it).

One item on ADR-0042 §5.3's deletion list was deliberately **kept**: the
multi-parameter `for`-loop save/clear/restore in `vm_for_loop_body.rs`. Only its
unscoped half was a workaround. A loop parameter is a fresh binding that shadows
a *lexically enclosing* typed `$v`, and implementing that shadowing is genuine
Raku semantics, not a patch over the map. Its comment now says so, and the
latent defect the ADR noted — the save reading env-first while the restore wrote
both stores, promoting an env-only constraint into a global one — is gone with
the second store.

## The one real bug the deletion exposed

Removing the map turned `Interpreter::env_type_constraint_seen` from an
optimisation into a correctness-critical gate: it short-circuits the
`format!` + `env.get` when no typed lexical has ever been declared, and with the
map gone a `false` reading means "no constraint" rather than "consult the map".
It was a per-interpreter field, and an interpreter that *adopts* another's env —
`throws-like`'s nested EVAL interpreter, the regex-scratch interpreters,
`clone_for_thread` — is constructed fresh, so it started `false` while holding
the very `__mutsu_type::*` keys it was meant to gate. `throws-like q[$foo =
'xyz']` against a `my Int $foo` stopped throwing (`roast/S02-types/type.t` 5-11,
`subset-6c.t`, `subset-6e.t`, `S09-typed-arrays/arrays.t`).

The flag is now a process-global monotonic `AtomicBool`, for exactly the reason
`ATOMIC_VAR_SEEN` already is: enumerating every env-adoption site is a
completeness-dependent design, and getting it wrong here trades a loud refusal
for a silent wrong answer. An over-set is conservative — it only makes the
correct lookup run.

This is also a note on measurement technique. The blast radius was first probed
with an env-gated build that made the map *reads* return `None`, and that
experiment came back green across the whole `t/` suite and 414 targeted roast
files. It was a false green: with the local flag still `false`, the gated read
never reached the code that would have exposed the problem. The four roast
failures only appeared once the field itself was gone. A gate flag is part of
the mechanism under test, not scaffolding around it.

## Verification

- `t/typed-constraint-store-matrix.t` — 46 assertions, byte-identical output
  under `mutsu` and `raku`.
- `make test`: 3700 files, 37887 tests, green.
- The full roast whitelist on the release binary: 1436 files, 218962 tests,
  green.
- Bundled-library gate (`scripts/battery-testsuite.sh`): `GATE PASSED`, 289/312.
