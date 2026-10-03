# Declaring a role no longer env-syncs every local

#10999 bounded the `needs_env_sync` fold for a `class` declaration, but a
`role` declaration still marked **every** local of its frame env-synced, so a
top-level loop paid an env mirror write on each store as soon as the file
declared a role (#11078).

`Compiler::note_role_decl_env_sync` (`src/compiler/lazy_body_env_sync.rs`)
now bounds a role plan the same way. On top of the channels a class
registration reads outer lexicals through — compiled method bodies, their
signatures' declaration-time expressions, attribute descriptors, trait
arguments and type names, now shared helpers between the two — a role adds:

* its type-parameter signature (`role R[$n = $outer]`), whose defaults are
  evaluated per parameterization;
* its `does R2[...]` parent arguments;
* the deferred body statements every composition runs. A nested type
  declaration already carries a compiled chunk; a plain statement is
  recompiled from raw AST under the composition's ambient package, so its
  reads are enumerated from an analysis compile of the same statement (the
  package changes how a name qualifies, not which lexicals it reads).

A `token`/`rule` in the body, a computed method name, or anything else still
evaluated from raw AST keeps the old every-local fold.

On the issue's repro (100,000 iterations of `$t = R.m; $i = $i + 1` after
`role R { method m() { 1 } }`, callgrind) `exec_set_local_op`'s inclusive Ir
is now 61,204,729 — identical to the same loop with the role removed and the
call replaced by a constant. The punned call itself turned out to cost ~200k
instructions per call for an unrelated reason, filed as #11115.
