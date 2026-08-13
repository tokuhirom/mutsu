# A caller's closure no longer loses its capture to a same-named callee parameter

The original repro filed in `todo/deep/closure-capture-shadowed-by-colliding-callee-parameter.md`
(a closure over a caller lexical `$s`, invoked from inside a callee whose own
parameter is also named `$s`) turned out to already be fixed by unrelated
work — the class of bug it named persisted, but through a different mechanism
than the doc's original trace pointed at.

## Root cause

A **named** closure-literal argument passed to a call whose callee is not
statically known at compile time (an imported routine, or any routine reached
through the parser's generic "unknown bareword + listop args" fallback —
`Stmt::Call`, not `Expr::Call`/`Expr::UserRoutineCall`) was compiled through
one of two code paths that never marked the closure literal as an *escaping*
value:

- `Compiler::compile_tail_stmt_call_value` (`src/compiler/helpers_control_flow.rs`),
  used when the `Stmt::Call` is the tail statement of its enclosing block.
- The "statement-level call with named args" branch of `compile_stmt`'s
  `Stmt::Call` arm (`src/compiler/stmt.rs`), used otherwise.

Both compiled a named argument's value with a plain `self.compile_expr(expr)`,
which inherits whatever `escaping_position` the *surrounding* statement
happened to leave set — almost always `false`. Every other call-compiling path
(`compile_expr_call_inner`'s named-args branch, `compile_call_arg_with_escape`)
instead computes escaping-ness per argument via `is_closure_literal_arg`, so a
closure-literal value like `:after-tap({ ... })` is recognized as something the
callee might *store* rather than invoke immediately, forcing its
captured-and-mutated free variables through the box-on-capture path
(`box_captured_lexicals`) into a shared `ContainerRef` cell.

Without that cell, the closure's capture of `$s` was a plain (unboxed) value.
When the closure was later invoked from a *nested block* inside the callee
that itself read a same-named local (e.g. the callee's own `$s` parameter,
still live in the calling frame's env chain), the call-time env merge
(`call_compiled_closure_with_topic` in `src/vm/vm_closure_dispatch.rs`)
defaults to `entry_or_insert_sym` — install-if-absent — for any free variable
that is neither a `ContainerRef` cell nor listed in the closure's
`authoritative_free_vars`. Since the calling frame already had a live `$s`
(the callee's own parameter), the closure silently kept reading *that* value
instead of its own captured lexical.

## The fix

`src/compiler/helpers_control_flow.rs` and `src/compiler/stmt.rs`: a closure
literal used as a **named** argument value in either `Stmt::Call` compile path
now compiles under `with_escape(is_closure_literal_arg(expr), ...)`, matching
every other call-compiling path. This is deliberately narrow — **positional**
arguments in these two paths keep the existing unconditional
non-escaping `compile_call_arg` treatment. Widening the fix to positional args
too regressed `t/bind-alias-chain.t` (`lives-ok { $b = 5 }, $desc` — a
positional block argument rewritten by `rewrite_stmt_call_args` into an
`AnonSub` — got boxed and desynchronized from the `$b := $w` bind-alias
group) and `t/for-quanthash-values-rw-writeback.t`; those regressions are the
concrete evidence that positional-arg escaping needs its own, separately
verified change, not a blanket widening.

## Minimal repro (now fixed)

```raku
use lib 'lib3';
use MyModule;   # exports `sub tap-ok($s, :&after-tap) { subtest { ...; after-tap() } }`

{
    my $s = Supplier.new;
    tap-ok $s.Supply, :after-tap({ $s.emit(1); $s.done });   # after-tap is a NAMED arg
}
```

The bug required the callee to be reached through the `Stmt::Call` path
(an imported/dynamically-resolved routine — a locally-declared `sub` in the
same file instead compiles the call via `Expr::Call`/`Expr::UserRoutineCall`,
which was never affected) *and* the closure to be passed as a named argument.

Pin: the existing `t/supply-unique-tap-ok-expires.t` continues to pass, and
was re-verified with its Supplier deliberately renamed back to `$s` (matching
`Test::Tap::tap-ok`'s own first parameter) to confirm the collision no longer
shadows the capture.
