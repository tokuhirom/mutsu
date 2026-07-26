# A class inside a module can see the module's subs

A method of a class declared inside a `module` could not call a sub declared at
that module's scope: the call died with `Unknown function: <name>`. raku
resolves it, because the class body's lexical scope *is* the module's.

```raku
unit module NL;
our sub cannon-name($libname) { "cn:$libname" }
class Searcher {
    method try-versions($libname) { cannon-name($libname) }   # Unknown function
}
```

This was the blocker behind five of the nine `DBIish` test files — `NativeLibs`'
`class Searcher` calls the module-scope `cannon-name` at lines 131 and 134 — but
it had nothing to do with `DBIish`, `NativeLibs`, `proto`/`multi` or NativeCall.
It was a plain lexical-scope bug, and the earlier reductions filed against it all
called from a *sibling sub*, which is the one shape that already worked.

## Root cause

Two things had to be true for a bare name to reach the enclosing module, and
neither was.

**The running package did not describe where the method was declared.** A method
body switched `current_package` only when its class declared its own subs or its
own `my` statics; otherwise it ran under whatever package the *caller* was in —
`GLOBAL` for a call from the mainline. So while executing
`NL::Searcher.try-versions`, the interpreter believed it was in `GLOBAL`.

**Bare-name lookup had exactly two scopes.** Every resolution path — the
`FunctionDef` registry, the multi-candidate collectors, the proto lookups, the
compiled-function key probes — was built from a hard-coded pair: the current
package, then `GLOBAL`. There was no step in between, so `NL::cannon-name` was
unreachable from any package other than `NL` itself. The file-scope case worked
only because there the enclosing package *is* `GLOBAL`, which is why this never
showed up in ordinary single-file tests.

## Fix

A class declared inside a package is registered under the package-qualified name
(`class Searcher` in `unit module NL` becomes `NL::Searcher`), so the name itself
records the lexical nesting. Method dispatch now anchors `current_package` to the
owning class whenever that name is package-qualified, and a new
`Interpreter::bare_name_packages` turns it into the ordered scope list a bare
name is searched in — innermost first, then each enclosing package, ending at
`GLOBAL` (`NL::Searcher` -> `NL` -> `GLOBAL`).

Every bare-name resolution path now walks that list instead of the old
current-package/`GLOBAL` pair: `resolve_function`, `resolve_function_with_types`
and its optional/slurpy/any-arity candidate collectors,
`resolve_function_with_arity`, `resolve_all_matching_candidates`,
`resolve_all_multi_candidates`, `resolve_proto_function`,
`resolve_remaining_proto_candidates`, the `has_proto` / `has_multi_candidates` /
`has_declared_function` / `has_multi_function` predicates, and the VM's
compiled-function key probe. Because the list is ordered innermost-first, an
enclosing module's routine now correctly shadows a same-named `GLOBAL` one.

Pinned by `t/class-in-module-sees-module-subs.t` (with
`t/lib/ClassSeesModuleSubs.rakumod`), which covers `our sub`, a lexical `sub`, an
`our constant`, a `proto`/`multi` pair, a doubly-nested class, a nested `module`
preferring its own sub, a mainline `module { ... }` block, and the
GLOBAL-shadowing order.

## Effect on `DBIish`

Five files moved off this blocker at once. Measured with `tmp/dbiish-survey.sh`,
debug build, both interpreters on the same `-I` line:

| File | before | after | raku |
| --- | --- | --- | --- |
| `03-lib-util` | ran 3/5, dies | 1 subtest fails | 1 subtest fails |
| `44-sqlite-memory` | ran 0/109, dies | 1 subtest fails | 1 subtest fails |
| `45-sqlite-common` | ran 0/109, dies | 1 subtest fails | 1 subtest fails |
| `46-sqlite-blob` | ran 0/18, dies | **PASS 18/18** | PASS 18/18 |
| `48-sqlite-errors` | ran 2/17, dies | **PASS 17/17** | PASS 17/17 |

The remaining `DBIish` blockers are tracked in
[`todo/tickets/dbiish-blockers.md`](../../todo/tickets/dbiish-blockers.md).
