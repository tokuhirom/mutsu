# A context EVAL, and an END phaser, run in the compunit they belong to

`main` went red on the `test` job's bundled-library gate with three regressions
that all reported the same kind of failure — a package-qualified name that would
not resolve:

```
REGRESSION: whitelisted 'DateTime::Parse	01-basic.t'   # Could not find symbol 'DateTime::Parse'
REGRESSION: whitelisted 'Encode	01-basic.t'            # Could not find symbol 'Encode::decode'
REGRESSION: whitelisted 'Log::Async	08-use.rakutest'   # Could not find symbol 'Terminal::ANSI::OO'
```

Two commits interact to produce them. #7797 gated package-qualified names by
which compunit `use`d the package, and #7830 made the vendored upstream `Test`
the default provider — which moved `throws-like`'s and `use-ok`'s `EVAL` out of
native Rust and into a real Raku compunit, where the new gate applies to it.
Neither is wrong; what was missing is that **several things that run "somewhere
else" still belong, lexically, to the compunit that wrote them**, and the gate
was reading the wrong one for two of them.

## A context EVAL inherits the context's compunit

`EVAL $code, context => $ctx` compiles the snippet as if it stood at `$ctx`'s
frame. mutsu already honoured that for the *package*, and for ADR-0037's
`return` classification — but not for the compunit, and a compunit is part of a
lexical scope just as much as a package is: which modules it `use`d is precisely
what decides the package-qualified names code written in it may use.

So the EVAL unit's parent was whichever compunit happened to be *running* the
`EVAL`. For upstream `Test.rakumod` that is `Test` itself:

```raku
my $caller-context = $*THROWS-LIKE-CONTEXT // CALLER::; # Don't guess our caller context, know it!
...
EVAL $code, context => $caller-context;
```

`Test` passes `CALLER::` for exactly this reason, and mutsu then ignored it and
compiled the snippet as if `Test` had written it. `Test` never `use`s the module
under test, so every package-qualified name in a `throws-like` string went
undeclared. The imported name resolving while the qualified one did not — same
sub, same scope — is the whole bug in one pair of assertions:

```
ok 1 - throws-like resolves the IMPORTED name
    # Got:      Could not find symbol 'Widget::make'
not ok 2 - throws-like resolves the QUALIFIED name
```

A `CALLER::`/`CALLERS::` pseudo-stash now carries its frame's compunit
(`STASH_ORIGIN_UNIT_ATTR`) beside the package and control-flow identity it
already carried, and `EVAL` uses it as the EVAL unit's parent. That fixes
`DateTime::Parse` and `Encode`.

`on_resolve`-style contract note: nothing else changes for a plain `EVAL`, which
keeps inheriting the running compunit — that IS its caller's lexical scope.

## An END phaser runs in the compunit that declared it

`EndPhaser` already recorded and restored the declaring **package**, for a
reason its own comment states: END bodies run at program exit, long after
`current_package` has returned to `GLOBAL`. The compunit needed exactly the same
treatment, and for exactly the same reason — the strongest rule in
`qualified_name_visible_here` is "a compunit always sees a package it declares
itself", and at exit `current_unit` names the main compunit instead.

This is invisible while the module is also visible from the main compunit, which
is why it took an `EVAL`-loaded module to surface: there the grant went to the
ephemeral EVAL unit, and nothing was left that could see the module's own
package. `Log::Async`'s

```raku
END {
    Log::Async.instance.done if Log::Async.instance;
}
```

reached through `use-ok` (which is literally `EVAL "use $module"`) therefore
died — and took the rest of the exit sequence with it, so the *script's own* END
phasers never ran either.

## What is still broken

`Log::Async/08-use.rakutest` still fails, on a third and separate facet of the
same "which compunit is this code from" question, now root-caused precisely:

A module's `sub EXPORT` records `FunctionDef::source_file` from `?FILE` as of
its registration, and when the load was reached through an `EVAL` run inside
**another module's routine**, that names the invoking compunit rather than the
module. `enter_compilation_unit` then anchors `current_unit` to it for the whole
call, so `Terminal::ANSI::OO`'s own `sub EXPORT` cannot see its own class:

```raku
sub EXPORT($t = 't') {
  %( $t => Terminal::ANSI::OO.new(:get-codes) )
}
```

The discriminator is sharp — the same `EVAL "use Terminal::ANSI::OO"` succeeds
from the main script's top level *and* from a sub declared in the main script,
and fails only from a sub declared in another module. Anchoring at the
`apply_module_export` call site does not help: `enter_compilation_unit` resets
`current_unit` from the `CompiledFunction` on entry, so the fix has to be to the
recorded `source_file` itself. #7836 carries the detail.
