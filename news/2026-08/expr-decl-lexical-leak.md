# An `if (my $x = ...)` in a callee no longer clobbers the caller's `$x`

An expression-position `my` declared inside a routine's condition —
`if (my $file = %args<file> :delete)`, `while my $line = ...` — compiles to
an env-only store (no local slot). Four separate return paths then treated
that store as a write to an *enclosing* lexical and propagated it, so calling
the routine silently overwrote a same-named variable in the caller:

```raku
sub f(*%args) { if (my $d = %args<d> :delete) { return 1 }; 42 }
my $d = "outer";
f();
say $d;   # was: (Any) — now: outer
```

Found while writing the `Text::CSV` battery smoke test: `Text::CSV`'s
`method csv` declares `my $file` exactly this way, and every `csv(...)` call
reset the caller script's `$file` to `Any`.

All four paths were general bugs, fixed independently:

1. **Free-var-write scan** (`opcode.rs`): the body scan recorded the
   declaration's `SetGlobal` as a free-variable WRITE, so the call-site
   writeback drain copied the callee's value into the caller's slot. Names in
   `expr_declared_syms` (this body's own expression-position declarations)
   are now excluded from `free_var_writes`.
2. **Routine-local collection** (`ast.rs`): `collect_routine_body_local_names`
   never walked `If`/`While` *condition* expressions, so the slow-path return
   merge did not know the name was callee-local. It now walks conditions the
   same way `collect_all_my_decl_names` always did.
3. **Method fast-path merge** (`vm_method_dispatch.rs`): one of the two
   `is_method_local` predicates consulted `env_only_decls`, the other did
   not. Both do now.
4. **Interpreter-carrier write log** (`vm_exec_dispatch.rs`): while a carrier
   (EVAL / interpreter fallback) is active, every by-name env write is logged
   for the carrier-return writeback. A DECLARATION is a fresh binding, never
   a caller write, so the `SetGlobal` handler now removes what its own
   vardecl store added to the log (an earlier genuine write of the same name
   stays logged).

Pin: `t/expr-decl-lexical-no-leak.t` (sub, method, and EVAL-carrier shapes,
plus the callee's own view of the binding). One sibling shape remains open —
when the caller's lexical is cell-boxed and the callee is a *method*, the
declaration writes through the captured cell:
`todo/tickets/expr-decl-writes-through-captured-cell.md`. (Originally recorded
as also affecting subs whose caller lexical was declared BEFORE them; the
2026-08-20 re-verification found that half already fixed.)
