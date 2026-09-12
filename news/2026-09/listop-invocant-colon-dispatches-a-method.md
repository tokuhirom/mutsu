# The listop invocant colon now dispatches a method everywhere

Raku's listop invocant colon makes the first argument the invocant: `foo $x:
@args` means `$x.foo(@args)`. mutsu's parser already built the right AST node
for this (`CallArg::Invocant`, `Expr::MethodCall`) for the *non-tail*
statement-level case, but several other places that compile a `Stmt::Call`
either dropped the invocant marker and called the listop as a plain function,
or hit `unreachable!()`:

- `compile_tail_stmt_call_value` (shared by several tail-position call sites,
  including a block/closure whose last statement is a call) panicked, because
  its positional/named-pairs split has no arm for `CallArg::Invocant`.
- `compile_routine_body_stmts` (a named `sub`'s own tail statement) and
  `compile_closure_body_with_routine_flag`'s value-producing branch both
  converted `CallArg::Invocant` into a plain positional argument, silently
  dropping the colon.

`die`/`fail` additionally had their own statement parser (`die_stmt`) and
their own expression-position arm in `identifier_call.rs`, neither of which
looked for a trailing `:` at all — `die "boom":` was a hard parse error
(`fail`, `warn`, `say`, `sort`, `return` all *parsed*, since they go through
the general listop-argument parser, but only the non-tail-statement case
dispatched correctly).

Fixed by adding one shared helper,
`Compiler::invocant_colon_method_call`, that turns `(name, args)` into the
equivalent `Expr::MethodCall` when `args` opens with an invocant, and wiring
it into the three compiler call sites above; and by teaching `die_stmt` (and
`die`/`fail`'s expression-position arm) to try the same no-paren
invocant-colon parse `say`/`print`/`put`/`note` already use
(`parse_io_colon_invocant_stmt`, renamed from an I/O-specific name since it is
the general no-paren invocant-colon shape) before falling back to their own
statement form.

`say $x:`, `sort $x:` and `return $x:` already produced the right answer by
coincidence — `Str`/`Array` really do have `.say`/`.sort` methods, and `.return`
is a real control-flow method mutsu already implements, so dropping the colon
and dispatching the method happened to agree. `warn $x:`/`die $x:` disagreed,
because `Str` has no such method: mutsu warned/died with the string instead of
raising `X::Method::NotFound` the way rakudo does. This let
`Math::FractionalPart`'s
`die "FATAL: ...; please report this issue":` load at all (previously a hard
parse error), which also unblocks `Astro::Utils` and `DateTime::Julian`
(both reach the construct through `Math::FractionalPart`).

`t/routines/call/call-invocant-colon-listop.t` pins all five listops from the
issue's table — the three that already matched rakudo by coincidence keep
matching for the *right* reason, and `warn`/`die` now raise
`X::Method::NotFound` like rakudo does.

Closes #8141.
