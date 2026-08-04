# A `sub` declared in a callable block stops being callable when the block ends

```raku
sub run(&c) { c() }
run { sub infix:["@"] ($a, $b) { 42 } }
say EVAL 'sub circumfix:["@", "@"] ($a) { $a }; @ 5 @';
```

rakudo prints `5`. mutsu died with `Confused. Two terms in a row`: the
`infix:<@>` declared inside the block was still in the routine registry when the
`EVAL` string was *parsed*, so `@ 5 @` read as an infix application with a
missing right operand instead of the circumfix the string had just declared.

A block-local declaration outliving its block is bad on its own, but this is the
sharper consequence — a stale registry entry does not merely stay callable, it
**changes the grammar the next `EVAL` is parsed with**, because
`Interpreter::collect_operator_sub_names` builds the EVAL parser's operator
pre-seed by walking the whole registry.

## What was measured, and what the ticket got wrong

Four probes narrowed it. Breaking on `collect_operator_sub_names` and reading
its return value directly (rather than guessing from `.defined`, which answers
`True` in both cases and would have misled) gives:

| form | pre-seed |
| --- | --- |
| `run { sub infix:["@"] … }` (callable block) | `["infix:<@>"]` |
| `{ sub infix:["@"] … }` (statement-level block) | `[]` |

So the **main parse was already correct** — the parser keeps a properly scoped
`SCOPES` stack and a block-local operator never reaches it — and the
statement-level block was already correct at runtime too, because
`OpCode::BlockScope` brackets it with `snapshot_routine_registry()` /
`restore_routine_registry()`. `gather` had already hit the same gap and worked
around it by wrapping its body in a `Stmt::Block`.

The ticket proposed a narrow fix: intersect the registry walk with the `&name`
bindings visible in `env`, on the premise that a block-local routine leaves a
registry entry but no visible `&name`. Measured, that premise is false — the
`&name` leaks too. The real difference was the missing scope boundary.

## The fix

A new `OpCode::RoutineScope { body_end }` brackets a bytecode range with a
routine-registry save/restore, and the closure-body compiler emits it around a
body that is not a routine and declares at least one routine
(`Compiler::stmts_declare_routines`, the same predicate `gather` uses).

Putting the boundary in the *body's own bytecode* rather than at the call
boundary matters: there are six `call_compiled_function_*` paths, and threading
a save/restore through all of them is exactly the shape of change where one
missed path leaves a bug that looks fixed on the first call and reappears on the
second. Inside the body, every caller gets it. The emit has to come before the
`hoist_sub_decls` pass, whose `RegisterDecl` ops must fall inside the range, and
the restore runs on the error path too, so a `die` or `return` escaping the body
still unwinds the declarations it made.

Ordinary block calls are untouched: the opcode is only emitted for a body that
literally contains a `Stmt::SubDecl`.

## Effect

`roast/S06-operator-overloading/sub.t` had been aborting after 24 of its 29
assertions. All 29 now run; the two that still fail (21 and 28) are the file's
*other*, independent blocker, already triaged in
`todo/tickets/operator-extension-name-error-classes.md` —
`X::Syntax::Extension::TooComplex` and `X::Syntax::Extension::Category` where
mutsu answers a generic parse error.

Pin: `t/block-local-routine-scope.t`. Its negative assertions go through `EVAL`
on purpose: naming an undeclared routine directly is a *compile-time* error in
rakudo, which would reject the whole pin file before it ran.

## What is still different from rakudo

rakudo rejects `run { sub zzz { 42 } }; zzz()` at compile time ("Undeclared
routine"); mutsu now rejects it at *run* time ("Unknown function"). Closing that
gap needs a compile-time routine symbol table, which is a much larger change
than this one and is not what the EVAL-parse bug required.
