# A `sub` declared inside a method body leaks into the enclosing global scope

Discovered while pinning ADR-0019 C6e-3c's nested-sub-in-method-body fix
(`t/nested-sub-in-method-compiled.t`). Reproduces on `main` independent of
that change — not a regression, a pre-existing general bug.

## Repro

```raku
class Scoped {
    method secret-holder() {
        sub secret() { 42 }
        secret();
    }
}
say Scoped.new.secret-holder();  # 42
my $leaked = try { secret() };
say $leaked.defined;             # True — should be False
say $leaked;                     # 42 — `secret` is callable at top level
```

Expected (raku): `secret` is lexically scoped to `secret-holder`'s body and
is not callable outside it — the `try` should catch an "Undeclared routine"
error and `$leaked` should be undefined.

## Likely cause

A `sub` nested inside a *top-level* routine is correctly scoped
(`t/nested-sub-reregistration.t`'s "nested sub does not leak out of its
scope" case passes). The leak is specific to a sub nested inside a *method*
body, so the difference is presumably in how `RegisterSub` resolves its
enclosing scope/package when executing from method dispatch vs. ordinary
routine dispatch — worth comparing `register_compiled_sub_decl`'s scope
handling between the two call paths (`call_compiled_method` vs. the ordinary
sub/closure call entries).

## Why deferred

Out of scope for the C6e-3c nested-sub-registration fix (which only made the
nested sub's own plan bytecode resolve so it registers body-less — an
orthogonal concern to whether it leaks). Needs its own investigation into
scope handling during method-body `RegisterSub` execution.
