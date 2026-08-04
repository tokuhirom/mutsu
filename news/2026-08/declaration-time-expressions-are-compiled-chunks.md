# Declaration-time expressions are compiled child chunks

ADR-0019 slice C5. A declaration carries expressions of its own, separate from the
routine body: the computed name of `sub ::($name) {...}` / `class ::(NAME) {...}`, and
the argument of every custom trait — `is native(LIB)`, `is symbol('sym')`,
`is labelled(PREFIX ~ '-x')`. Until now the compiler stored those as `Expr`s inside the
declaration plan and the runtime built bytecode for them *at every registration*, by
wrapping the expression in a one-statement block and calling
`Interpreter::compile_block_value`. A NativeCall binding declares `is native(LIB)` on
every one of its entry points, so a module like `DBDish::mysql::Native` re-ran the
compiler roughly forty times on load for expressions that never change.

The compiler now lowers each of those expressions once, into a `CompiledDeclExpr` — a
`CompiledCode` plus its own compiled-function table — stored in the declaration plan.
Registration runs it through the VM's ordinary re-entrant bytecode entry
(`Interpreter::run_decl_expr`), which is the `vm_eval_block_value` fast path with the
on-demand compile removed: the same `run_nested` call and the same scope bookkeeping
(block-scope depth, `let`/`temp` restore, pending `DESTROY`). A trait argument that is
already a constant is recorded as a `DeclTraitArg::Literal` and needs no chunk at all,
which is the common case for `is symbol('...')` and for internal markers such as
`__mutsu_declare_how` (whose keyword the `EXPORTHOW::DECLARE` protocol reads straight
off the literal instead of pattern-matching an `Expr::Literal`).

The chunk is compiled the way the runtime helper it replaces compiled it — a standalone
unit with no local slots, so every variable the expression names resolves through the
environment the declaration registers in. What changes is that the package and
distribution context now come from the declaration's own lexical position rather than
from whichever routine frame happened to be live when registration ran.

The change covers sub, class, and role declarations. `DeclTraitArg` retains an `Ast`
variant for the two registration paths that still walk a source declaration — the
prelude's forward-declaration pass and the class/role *method* walkers — so this is the
existing fallback narrowed to those callers, not a new one; both disappear with ADR-0019
phase D.

Pinned by `t/decl-trait-arg-expression.t` (literal, computed, and call-valued trait
arguments on a sub, a class, and a role) on top of the existing
`t/indirect-declarator-names.t` and `roast/S02-names/indirect.t` coverage of computed
declaration names.
