# `EVAL`'s `context` argument is ignored, so the EVAL'd unit compiles in the wrong package

`EVAL $code, context => $ctx` compiles the string as if it stood at `$ctx`'s
frame. mutsu's `builtin_eval` (`src/runtime/builtins_eval_misc.rs:252`) reads the
`lang` and `check` named arguments and **never looks at `context` at all**, so
the code is always compiled in whatever `current_package` happens to be — which,
when EVAL is called from inside a module, is the module's package.

```
$ cat tmp/core/FatalMod.rakumod
use MONKEY-SEE-NO-EVAL;
unit module FatalMod;
sub run-it($code)    is export { my $ctx = CALLER::; EVAL $code, context => $ctx }
sub run-plain($code) is export { EVAL $code }

$ raku  -I tmp/core -e 'use FatalMod; run-plain(q{my class Foo { method a() { $!bar } }})'
Attribute $!bar not declared in class FatalMod::Foo      # plain EVAL: module package
$ raku  -I tmp/core -e 'use FatalMod; run-it(q{my class Foo { method a() { $!bar } }})'
Attribute $!bar not declared in class Foo                # context => CALLER:: -> caller's package
$ mutsu -I tmp/core -e 'use FatalMod; run-it(q{my class Foo { method a() { $!bar } }})'
Attribute $!bar not declared in class FatalMod::Foo      # wrong
```

**mutsu's plain-`EVAL` behaviour already matches raku** — the divergence is only
the `context` form.

## Why it matters now

rakudo's `Test.rakumod` uses exactly this form for the string version of
`throws-like`:

```raku
my $caller-context = $*THROWS-LIKE-CONTEXT // CALLER::;
subtest { ... EVAL $code, context => $caller-context; ... }
```

So every `throws-like '<code that declares a class>', X::...` assertion sees the
class named `Test::Foo` instead of `Foo`. In the Test-vendoring sweep
(`todo/tickets/vendor-real-test-module.md`) this is one of the two *systemic*
causes left in the 30 real-gap files — `t/attribute-undeclared.t`
(`.package-name` is `Test2::Foo`) and `t/composition-not-composable.t`
(`.target-name` is `Test2::B`) fail on it, and any other file whose EVAL'd
snippet declares a package will too.

## Why it is not a small fix

The obvious shortcut — "when `context` is given, use the package one routine
frame up" — is wrong. `CALLER::` names the frame that was current *where the
stash was written*, and `Test.rakumod` writes it in `throws-like` but uses it
several frames deeper, inside the `subtest { ... }` block. Reading the routine
stack at EVAL time therefore picks a different frame than the one the stash
means.

A correct fix needs the **pseudo-stash value to record the frame it was taken
from**. Today it does not: `CALLER::` produces a plain `Stash` instance whose
`name` attribute is the literal string `"CALLER"` (`make_stash_instance`,
`src/runtime/accessors_stash.rs:37`; built by `OpCode::GetPseudoStash` ->
`exec_get_pseudo_stash_op`, `src/vm/vm_exec_dispatch.rs:709`), with no link back
to a routine frame. `RoutineFrame` (`src/runtime/mod.rs:746`) does carry a
`package`, so the information exists at construction time — it is simply not
captured.

Sketch of the work:

1. Give the pseudo-stash produced for `CALLER::` / `CALLERS::` / `MY::` /
   `OUTER::` a record of its originating frame (at minimum that frame's
   package; the lexical half is already handled separately, see
   `news/2026-07/...` and the `#4435` EVAL/`CALLER::` frame semantics).
2. Have `builtin_eval` read `context`, resolve that package, and set
   `current_package` around the compile+run of the EVAL'd unit (restoring it
   afterwards, including on the error path).
3. Decide what a *non*-pseudo stash context (`Foo::`) means — raku compiles in
   that package, which falls out of the same mechanism.

Pin candidates: the two `t/` files above under the real `Test` module, plus a
direct `t/eval-context-package.t` built from the `FatalMod` repro, which is
green under `raku`.
