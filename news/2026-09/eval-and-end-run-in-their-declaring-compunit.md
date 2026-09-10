# Code runs in the compunit it was written in: EVAL context, END phasers, module routines

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

Nothing changes for a plain `EVAL`, which keeps inheriting the running
compunit — that IS its caller's lexical scope.

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

## A module's own routines must be stamped with the module's file

`Log::Async/08-use.rakutest` needed a third fix, of the same shape.

A routine records its declaring file **as it is created**, via
`executing_source_file()` — which walks the routine stack for the innermost
frame's `def_file`. But a module's own top-level mainline runs through
`run_block` and pushes **no routine frame**, so that walk goes straight past it
to whatever routine is still on the stack underneath. When the load was
triggered from a string `EVAL` inside another module's routine — `use-ok` is
literally `EVAL "use $module"` — that frame belongs to a different compunit
entirely, and every `sub` the loaded module declares got stamped with the
*invoking* compunit's file.

`enter_compilation_unit` then anchors `current_unit` from that stamp on every
call, so `Terminal::ANSI::OO`'s own EXPORT could not resolve its own class:

```raku
sub EXPORT($t = 't') {
  %( $t => Terminal::ANSI::OO.new(:get-codes) )   # Could not find symbol
}
```

Measured at the registration site, the two available answers disagree exactly as
that predicts, and the correction already designed for #7797's sibling problem
applies unchanged:

```
REGSUB name=EXPORT
  exec_sf = tmp/endlib/UseOkish.rakumod                       <-- stale caller frame
  ?FILE   = modules/Terminal-ANSI/lib/Terminal/ANSI/OO.rakumod <-- correct
  rstack  = 2   modstack = (Terminal/ANSI/OO.rakumod, 2)      <-- depths match
```

`declaring_source_file()` is the declaration-side twin of
`executing_unit_sym_for_module_load()`, and uses the same test for the same
reason: `routine_stack.len()` unchanged since the innermost still-loading
module's mainline started means nothing has been *called* since, so that module
is what is running and `?FILE` — which `load_module_inner` scopes to the module
path for exactly this window — is authoritative. A change means a routine call
happened and the frame-based answer is right again, which is what keeps a
closure literal built each time an *already-loaded* module's routine runs
attributed to its own file rather than to a stale `?FILE`. The five sites that
stamp a routine or closure with its declaring file now use it.

## Verification

Three focused regression tests under `t/modules/`, each verified against rakudo.
`scripts/battery-testsuite.sh` — the gate that caught all of this, and which
neither `make test` nor `make roast` exercises — is back to `285/311` with all
three regressions gone; the only files still failing there are the six
`DBIish` mysql ones, which need a `libmysqlclient` this container has not got
(`NativeCall: symbol 'mysql_init' not found ... dlsym failed`) and which CI
installs.

This branch also carries the #7831 supply-ticket fix (PR #7834): without it
`t/supply-serialize-fifo.t` test 3 fails 17-37% of runs, and the `t/` suite is
fatal in CI, so an unrelated red would be a coin flip on every push here.
