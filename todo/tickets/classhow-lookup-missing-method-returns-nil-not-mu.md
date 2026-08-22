# `.^lookup("nonexistent-method")` returns `Nil` instead of the `Mu` type object

Discovered via the doc-diff harness on `raku-doc/doc/Type/Metamodel/MethodContainer.rakudoc`
(around line 73 — bucketed `raku-drift` overall because the doc's other two example lines have
since drifted from current raku's actual signature-gist format, but this specific line is a
real, separate bug independent of that drift).

## Minimal repro

```raku
say Int.^lookup("does-not-exist");
```

- `raku`: `(Mu)`
- `mutsu` (`target/debug/mutsu`): `Nil`

## Root cause hypothesis

`.^lookup` (the `Metamodel::MethodContainer` reflection method that looks up a method by name
and returns its `Method` object, or an "absent" marker when not found) presumably falls back to
mutsu's internal `Nil` value on a failed lookup, where raku's `MethodContainer.lookup` returns
the `Mu` type object (the universal "no value" type object) for a not-found name. This looks like
a narrow, single-site fix — return `Value::mu_type_object()` (or however mutsu represents the
bare `Mu` type object elsewhere) instead of `Value::NIL` from the not-found branch — rather than
being related to the broader Nil-vs-Any identity knot tracked as Deferred (that cluster is about
*scalar* Nil/Any rendering, not about a specific reflection method's "not found" sentinel).

## Affected files (starting point)

- `src/runtime/methods_classhow_dispatch.rs` — the `.^lookup` implementation's not-found path
