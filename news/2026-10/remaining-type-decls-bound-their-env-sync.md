# Grammars, computed names, hoisted and nested types no longer env-sync every local

#10999 and #11078 bounded the `needs_env_sync` fold for a class and a role
declaration, but several shapes still marked **every** local of the
declaring frame env-synced, so each store in a top-level loop paid an env
mirror write (#11116). `src/compiler/lazy_body_env_sync.rs` now bounds them
too:

* **A `token`/`rule` in a class or role body** (so any grammar). The regex
  body is interpreter-executed (ADR-0009) and has no ops to scan, but a
  static pattern — no variable interpolation, `"..."` thunk, code block or
  `&` call — reads nothing by name except a subrule `<foo>` that may resolve
  a lexical `&foo`, and every identifier of the pattern is folded in as
  such. An interpolating token keeps the old fold.
* **A computed name** (`class ::($n)`, `method ::($n)`) and **expressions
  still evaluated from raw AST at registration** (a fallback trait
  argument, an attribute's unknown-trait argument, a method trait
  argument). Their reads are enumerated from an analysis compile of the
  same expression, body or statement — the same technique the signature
  defaults already used. A method that is compiled only at registration is
  analysis-compiled the same way.
* **A `__hoisted` forward-reference shell.** The shell registers a subset of
  its source-order declaration's body, so when that declaration is bounded
  its shell (matched by `decl_id`) is marked bounded with it.
* **A class or role nested in a routine body.** A bounded nested type's
  reads were resolved only against the routine's own slots and then lost on
  the way out. They are now also kept in `CompiledCode::lazy_decl_reads`,
  which the enclosing declaration's bound folds in, so a bounded nested type
  no longer makes its enclosing sub or class unbounded.

The scan helpers moved to `src/compiler/lazy_body_reads.rs`.

On a 100,000-iteration loop storing into two locals (callgrind,
`exec_set_local_op` inclusive Ir), a frame declaring
`class C { token t { a }; method m() { 1 } }` and one declaring a class whose
method nests `my class In { }` now both cost 65,404,771 — identical to the
same loop with no declaration — while an interpolating `token t { <$t> }`
still pays the full fold (159,605,346).
