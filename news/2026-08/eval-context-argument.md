# `EVAL`'s `context` argument compiles the snippet in the caller's package

`EVAL $code, context => $ctx` compiles the string as if it stood at `$ctx`'s
frame. mutsu's `builtin_eval` read the `lang` and `check` named arguments and
never looked at `context` at all, so the snippet was always compiled in whatever
`current_package` happened to be — which, when EVAL is called from inside a
module, is the module's own package:

```
$ mutsu -I lib -e 'use FatalMod; run-it(q{my class Foo { method a() { $!bar } }})'
Attribute $!bar not declared in class FatalMod::Foo      # raku: class Foo
```

mutsu's *plain*-`EVAL` behaviour already matched raku (a package the snippet
declares does belong to the calling module); the divergence was only the
`context` form.

## Why the obvious shortcut is wrong

"When `context` is given, use the package one routine frame up" does not work.
`CALLER::` names the frame that was current *where the stash was written*, and
rakudo's `Test.rakumod` writes it in `throws-like` but uses it several frames
deeper, inside the `subtest { ... }` block:

```raku
my $caller-context = $*THROWS-LIKE-CONTEXT // CALLER::;
subtest { ... EVAL $code, context => $caller-context; ... }
```

Reading the routine stack at EVAL time therefore picks a different frame than
the one the stash means. The value has to remember the frame it came from.

## What changed

The pseudo-stash produced for `CALLER::`/`CALLERS::` now carries the package of
the frame it was taken from, as an attribute rather than as a member of its
`symbols` hash — so it stays invisible to `.keys` and `.gist`, and only
`builtin_eval` reads it back. `caller_frame_package()` answers the frame one
below the currently-executing one; a block frame carries the package its closure
was created in, which is what makes a `subtest { ... }` body written in a test
script answer the script's package rather than the module's. With no caller
frame at all the caller is the mainline, whose package for a script is `GLOBAL`.

`builtin_eval` then resolves `context` to a package — the recorded origin for a
pseudo-stash, the package itself for a real one (`Foo::`) or a `Package` value —
and sets `current_package` around the compile and run of the EVAL'd unit,
restoring it on every path so the context cannot leak into a later plain `EVAL`.

## Why it mattered

This was one of the two systemic causes left in the campaign to run rakudo's
real `Test.rakumod` verbatim (`todo/tickets/vendor-real-test-module.md`): every
`throws-like '<code that declares a class>', X::...` assertion saw the class
named `Test::Foo`. Under the temporary alias it frees `t/attribute-undeclared.t`
(`.package-name` was `Test2::Foo`) and `t/composition-not-composable.t`
(`.target-name` was `Test2::B`).

Pinned by `t/eval-context-package.t` with `t/lib/EvalContext.rakumod`, green
under `raku`, mutsu's native `Test` and the aliased upstream module alike.
