# Fixed a `run_class_body` early-return leaking the runtime's current package

A class-body statement that failed mid-registration (an early `Err` from
anywhere in `Interpreter::run_class_body`'s per-op walk) used to permanently
corrupt the interpreter's runtime `current_package`, silently mis-qualifying
every class registered afterward in the same compilation unit. The reported
symptom was a `trusts` declaration inside a nested class body making an
unrelated sibling class report a package-qualified `.^name`:

```raku
class Plain { }
say Plain.^name;          # was: Outer::Plain    should be: Plain
class Outer {
    our class Inner {
        trusts Outer;
        method !secret() { 'from Inner' }
    }
    method poke() { Inner.new()!Inner::secret() }
}
say Plain.^name;          # was: Outer::Plain    should be: Plain
say Outer.poke;           # from Inner (this part was always correct)
```

## Root cause

`run_class_body` sets `self.current_package` to the class being registered,
walks its `body_plan`, then restores the saved package at the end. The walk's
per-op loop propagated any `Err` via `?` straight out of the function,
bypassing that restore entirely.

The concrete trigger: `Outer`'s declaration is preceded by a runtime statement
(`say Plain.^name;`), so `hoist_type_decl_shells` pre-registers a
forward-reference *shell* of `Outer` before the mainline even starts running.
The shell keeps `Outer`'s own `method poke() { ... }` (methods survive shell
extraction) but drops the *nested* `class Inner { ... }` declaration entirely
(`type_decl_shell_body` only special-cases `has`/`method`/`does`/`trusts`
statements — a nested `ClassDecl` falls through its catch-all and is dropped).
So when the shell registers `Outer` early and walks `poke`'s body,
`validate_private_access_in_stmts` sees the qualified private call
`Inner.new()!Inner::secret()` and tries to resolve `Inner` — which does not
exist in the registry yet, because it was never part of the shell. That
lookup fails, `class_body_method_decl` returns `Err`, and the error propagates
out of `run_class_body`'s loop without ever reaching
`self.set_current_package(cx.saved_package.clone())`. `exec_register_decl_op`
swallows the error for a `__hoisted` shell registration (by design — a failed
forward-reference shell is expected to fail sometimes) and moves on, but the
runtime package is now permanently stuck on `"Outer"` for the rest of the
file.

Because the corrupted class was later registered normally too (its
`RegisterDecl` op used the same, by-then-already-wrong package as its own
"saved" package to restore to), the leak was permanent, not just a transient
window — every class declared afterward, and any `type_metadata` lookup keyed
by name (e.g. `:ver`/`:auth`/`:api`), silently resolved under the wrong,
`Outer`-prefixed key.

## Was this specific to `trusts`?

No — despite the ticket's stated three-way trigger (nesting + `trusts` +
qualified call), re-verifying the matrix found `trusts` was not actually
required: dropping it still produced the same `Plain` -> `Outer::Plain`
corruption (the private-access check fails identically whether `Inner`
declares `trusts` or not, since `Inner` simply doesn't exist yet at
shell-registration time). What *was* required, confirmed by re-testing each
leg independently:

- a class nested inside another class body whose own declaration is dropped
  from the enclosing class's hoisted shell, and
- a *qualified* private call (`$o!Owner::method()`) inside a shell-surviving
  method body, referencing that not-yet-registered nested class.

An *unqualified* `self!secret()` call never triggers
`validate_private_access_in_stmts`'s cross-class resolution at all (it only
fires when the call name contains `::`), so that variant was correctly
unaffected. This is a general bug in `run_class_body`'s error handling, not
anything about `trusts` specifically: any class-body statement that can fail
mid-walk (a duplicate method definition, an undeclared attribute, any future
validation) had the same latent hazard.

## Fix

`run_class_body`'s per-op loop (plus the trailing LEAVE-phaser/statics calls)
is now wrapped in a closure so every exit path — success or an `Err` from any
op — funnels through one unconditional cleanup: the runtime package is always
restored to `cx.saved_package`, and on an error the lexical `env` is rolled
back to `cx.saved_env` too (mirroring the one call site that already did this
manually for the BUILD/TWEAK undeclared-attribute check).

## Tests

`t/trusts-nested-lexical-class.t` (previously kept separate specifically to
avoid this bug contaminating unrelated assertions) was merged back into
`t/metamodel-introspection.t`, which also gained new regression cases: the
sibling-class-name leak itself, the trigger matrix (nesting/`trusts`/qualified
call each dropped in turn), and the `:ver` metadata-lookup breakage this bug
caused. All 54 assertions in the merged file pass under both `raku` and
`mutsu`.
