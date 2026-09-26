# `ExecCallPairs` is gone: every statement call is the expression form's call

#9448 retired `OpCode::ExecCall`. This entry retires its sibling,
`OpCode::ExecCallPairs` (#9462).

A statement call that carried named arguments or a slip went through
`ExecCallPairs`. Examples are `ok 0, :todo(1);`, every Test assertion with its
parser-injected callsite-line pair, and a tail-position call such as
`to-json($ret, :$pretty)`. `ExecCallPairs` was a third copy of call dispatch.
It had its own:

- frame-lexical probe;
- EXPORT-hook check;
- compiled and native probes;
- carrier fallback (`exec_call_sanitized`) with a caller-env snapshot and
  writeback diff.

It had also drifted from the other copies. Unlike every other call arm, it did
not record a resume point, so a `warn` raised inside such a call could not be
`.resume`d.

Now `Stmt::Call` compiles to `Expr::Call`, which is the same `CallFunc` /
`CallFuncNamed` the expression form uses:

- A plain statement call is followed by `SinkPop(false, true)`.
- A tail-position call leaves its value on the stack as the body's result.

The opcode, its handler, and the helpers only it reached are deleted, about
690 lines in all:

- `exec_call_pairs_values_sanitized` and `exec_call_sanitized`;
- `snapshot_carrier_overwritable_env` / `carrier_writeback_changed_aggregates`
  and their two private helpers;
- the compiler's `add_call_arg_sources_constant`.

Once the carrier was gone, several bugs it had been hiding surfaced. The fixes
for them are part of this change:

- **An all-named sub accepted surplus positionals.** `sub d(:$x) {}; d(1, 2)`
  ran instead of dying. The named-light call path only checked arity for mixed
  signatures. It now also rejects surplus positionals for an all-named
  signature, with Rakudo's "Too many positionals passed; expected 0 arguments
  but got 2".
- **The sub form of `push`/`append`/`unshift`/`prepend` dropped a named
  argument.** `push @a, a => 52` silently discarded the argument. The compiler
  rewrites these calls to the method form, which ignores a named argument
  through its implicit `*%_`. The sub form now dies with "Unexpected named
  argument 'a' passed" (Rakudo: "Cannot resolve caller push(Array:D,
  :a(Int))").
- **`S///`, `s///` and `tr///` read the wrong `$_` inside a placeholder
  closure.** They work on the implicit topic without naming it, so closure
  capture never recorded `$_`. In `{ S{$^a} = 'X' }` the placeholder leaves the
  outer `$_` as the topic, but a lazy `.map` reified later substituted into
  whatever `$_` the running frame had. `compute_free_vars` now counts these ops
  as references to `$_`.
- **An EXPORT wrapper recursed into itself.** A module's `sub EXPORT` can export
  a wrapper `-> |c { name(|c, :from<wrapped>) }` under the inner routine's own
  name (#8746). The wrapper's call to `name` recursed forever, because
  `CallFunc` applied the EXPORT-installed `&name` override even to code inside
  the exporting module itself, where the bare name still means the module's own
  import. The expression form already failed this way on `main`. The override
  now applies only outside the unit that declared the wrapper
  (`callable_declared_in_unit_of`).
- **`callsame` in a metamodel HOW method answered an enclosing multi.** A user
  method on a `Metamodel::ClassHOW` subclass, such as OO::Monitors'
  `MonitorHOW.new_type`, defers to the native metamethod. That base candidate
  is not a `MethodDef`, so the method pushed no dispatch frame. Its `callsame`
  therefore resolved against any live frame of an enclosing routine. Calling
  `use-ok 'Terminal::ANSI'`, which is Test's `multi sub use-ok` and so now
  pushes a multi frame, broke the `monitor` declaration. The method now always
  gets a frame of its own, like the `new`/`BUILDALL` overrides already did.
- **A block's write to a caller lexical was taken by a deeper frame.** Once
  a closure writes a caller's variable, a pending entry tells the frame that
  owns the slot to refresh it. That entry was a bare name. So the first frame
  to drain with a local of that name took it, including a frame entered later
  and deeper. Test's `proclaim($cond, $desc is copy)` did that to the `$desc`
  written by the block in `lives-ok { $desc = ... }`, so the write was lost.
  A call frame now hides the pending entries while it runs and merges its
  own unclaimed entries back when it returns. A frame entered after the write
  therefore never sees it.
- **TRIR bound an `Int` to a native `num` parameter.** `sub f(num $x) { $x }`
  returned 5 for `f($five)`, while the same signature with a body that TRIR
  declines died. Rakudo rejects an `Int` there ("This type cannot unbox to a
  native number"), and TRIR now declines to bind it.

One difference between the statement and expression forms is kept on purpose,
and is filed as #9488. A statement call with named arguments still compiles
its *positional* closure-literal arguments non-escaping. That covers every
Test assertion, which carries a callsite-line pair. A closure compiled
escaping captures its mutated locals in shared cells, and those cells lose the
variable's container traits. With escaping capture, `dies-ok { %m<a> = 666 }`
on an `is Map` hash stops dying, and `lives-ok { $a = Nil }` stops restoring an
`is default`. The expression form `my $r = dies-ok { ... }` is already wrong
for that reason. The one-shot compiler flag
`stmt_call_positional_closures_nonescaping` carries a TODO to #9488.

`tests/execcallpairs_resolves_once.rs` pinned the old opcode's internals. It
becomes `tests/statement_call_resolves_once.rs`, which checks that a Test
assertion resolves its routine at most once per call, reports its own
callsite line, and binds its named arguments.

Regression test: `t/routines/signature/named-arg-statement-call.t` checks:

- a `CONTROL { .resume }` around a named-arg statement call that `warn`s;
- `lives-ok { %h<a> = 42 }` writing through to the caller;
- a tail-position named-arg call as the routine's value;
- a slip and a named argument in one statement call;
- that a non-tail call leaves no value behind;
- the first three hidden bugs above.

`t/oo/mop/metamodel-how-callsame-under-outer-multi.t` pins the metamodel
`callsame` case, `t/vm/writeback/caller-writeback-not-claimed-by-deeper-frame.t`
the pending-writeback case, and `t/nativecall/trir-native-num-param-rejects-int.t`
the native `num` case. Tests 9-12 of `t/routines/dispatch/dispatch-control.t`
wrapped `callsame` in `dies-ok`. That is itself a `multi sub`, so the
`callsame` found a dispatcher and did not die, in raku as well. They now probe
with a plain `try`.

The existing `t/modules/import-export/custom-export-sub-wraps-same-name.t` pins
the EXPORT-wrapper case.
