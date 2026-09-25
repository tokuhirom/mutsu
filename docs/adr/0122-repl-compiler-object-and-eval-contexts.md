# ADR-0122: `nqp::getcomp("Raku")` is a Raku-level compiler object, and a REPL context is a captured unit environment plus its lexical routines

- **Status**: Accepted (user approval 2026-09-25; implemented in #9366)
- **Deciders**: tokuhirom, Claude
- **Context**: [#9349](https://github.com/tokuhirom/mutsu/issues/9349). Touches
  [ADR-0037](0037-eval-context-frame-owns-the-return-target.md) (how an `EVAL` unit classifies its
  enclosing routine), [ADR-0096](0096-batteries-adoption-policy.md) (no native re-implementations
  of ecosystem modules), [ADR-0113](0113-frame-lexical-inner-subs.md) (frame-lexical subs).

## 1. Context

Four distributions (CodeUnit, Text::CodeProcessing, Jupyter::Kernel, Jupyter::Chatbook) died on
`Unsupported nqp:: op: nqp::getcomp`. None of them wants the op for its own sake: all four build a
persistent REPL session on the protocol rakudo's own `REPL` class uses.

```raku
my $*CTXSAVE  := self;                 # anything with a `ctxsave` method
my $*MAIN_CTX := $!context;            # what the previous line left behind
my $value := $compiler.eval($code, :outer_ctx($!context), :interactive(1));
$!context := $*MAIN_CTX;

method ctxsave { $*MAIN_CTX := nqp::ctxcaller(nqp::ctx); $*CTXSAVE := 0 }
```

In rakudo, the compiled mainline of every unit ends with `$*CTXSAVE.ctxsave()` when one is armed,
so `nqp::ctxcaller(nqp::ctx)` inside `ctxsave` is the unit's own frame; the next `.eval` compiles
its code with that frame as the outer lexical scope. So a session needs:

1. a compiler object with `.eval(code, :outer_ctx)`, `.version_string` and `.repl-mode`;
2. a first-class, re-enterable context: the lexicals, subs and operators an earlier unit declared
   must be visible to a later unit — and must not leak into the host program;
3. rakudo's core `REPL` class (Text::CodeProcessing and the Jupyter sandboxes call
   `REPL.new($compiler, {})`, `.repl-eval` and `.input-incomplete` on it).

mutsu has `EVAL` but no frame object: `EVAL` runs a unit in the caller's env and rolls back
everything the unit declared when it returns (its `my` lexicals, its `&name` bindings, the routine
registry).

## 2. Decision

### D1. The compiler and `REPL` are ordinary Raku classes

`Perl6::Compiler` and `REPL` are Raku source (`runtime/repl_compiler_prelude.rs`), registered in
`GLOBAL` the first time `nqp::getcomp("Raku")` runs or the bareword `REPL` / `Perl6::Compiler` is
read. Their methods are real methods, so `.^methods`, `.can` and overriding work, and there is no
new native method dispatch (CLAUDE.md's "no new slow path"). The single primitive behind them is
`__mutsu_compiler_eval($code, $outer_ctx, $ctxsave)`. `REPL` is adapted from rakudo's
`src/core.c/REPL.rakumod`, keeping the surface ecosystem code calls and leaving out the
interactive loop and the line-editor mixins, which are `src/repl.rs`'s job.

This is core-setting behaviour (`nqp::getcomp`, `REPL`), not an ecosystem module, so ADR-0096's
rung-3 ban does not apply: the four distributions run verbatim.

### D2. A context is a side-table entry named by a `BOOTContext` handle

`EvalContext { env, caller, routines, infix_ops }` lives in `Interpreter::repl_compiler`; Raku sees
a `BOOTContext` instance carrying its index. `nqp::ctx` snapshots the current (flattened) env and
records where `nqp::ctxcaller` of it leads; `nqp::ctxlexpad` answers the context's user lexicals as
a hash.

### D3. What a unit leaves behind is captured just before its scope unwinds

`compiler_eval` arms a capture for exactly one unit and runs it through the ordinary `EVAL`
pipeline (`builtin_eval`), with:

- the unit's env seeded from the outer context's env (or an empty env: without `:outer_ctx` a
  unit sees the setting only, as in rakudo — not the lexicals of whatever method called `.eval`);
- the outer context's routines and user operators installed in the registry for the unit's
  duration, then rolled back;
- `pending_eval_context_routine = Mainline`, since a REPL unit is a compilation unit's mainline
  whatever Raku method called `.eval`.

`eval_block_value_inner` calls `capture_eval_unit_scope` at the point it is about to unwind a
body's scope; for the armed unit (matched by `block_scope_depth`, so a nested `EVAL` never
captures) that records the env and the routine/operator delta against the registry as it stood
when the unit started. That is the moment rakudo's mainline calls `ctxsave`. The unit's `$_`,
`$/`, `$!` and the EVAL bookkeeping keys are dropped from the captured env.

After the unit returns, `compiler_eval` stores the new context and, if the `$*CTXSAVE` the caller
armed can `ctxsave`, calls it with `ctxsave_unit` set so that `nqp::ctxcaller(nqp::ctx)` inside it
answers the new context.

## 3. Consequences

- CodeUnit's own test goes from dying to 5/6, including its `todo` operator test, which rakudo
  2026.07 itself still fails. The sixth needs the numeric-context uninitialized warning
  ([#9359](https://github.com/tokuhirom/mutsu/issues/9359)).
- Text::CodeProcessing and the Jupyter sandboxes get past `getcomp` and `REPL`, and are then
  blocked by `my \_` sharing storage with the topic
  ([#9358](https://github.com/tokuhirom/mutsu/issues/9358)).
- A later unit's closure over an earlier unit's lexical sees the value as of its own context
  snapshot, not a shared container: contexts copy the env, they do not alias frames. A REPL line
  that mutates `$a` is visible to every later line (each line runs in the previous line's
  captured env), which is the behaviour sessions rely on.
- Contexts are never freed (a `TODO` in `runtime/repl_compiler.rs`): one per REPL line is fine
  interactively; a long-running sandbox grows without bound until contexts are GC-traced values.
- `nqp::ctx`/`nqp::ctxcaller` outside the `ctxsave` protocol are best effort: they read the
  caller frame from `caller_env_stack`, which not every call path pushes.

## 4. Alternatives rejected

- **Running each unit in the host's env and simply not rolling back.** Leaks every REPL
  declaration into the program that hosts the sandbox, and lets a unit see the calling method's
  `self` and parameters.
- **Native methods on a Rust-side compiler value.** Would add exactly the slow-path method
  dispatch CLAUDE.md forbids, and hide the methods from introspection.
- **Real first-class frames.** The correct long-term shape (a context would alias the unit's
  containers instead of copying them), but it needs the frame/env design to change; the copy is
  sound for the REPL protocol and keeps the change local.
