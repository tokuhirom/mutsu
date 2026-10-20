# An `EVAL`'s top-level `use` binds in a lexical import set

What an `EVAL`'d unit's own top-level `use` imports is lexical to that
`EVAL`. Its routines, multis and operators now bind in a lexical import set
of the `EVAL` unit, as a block's `use` binds in the block's set (ADR-8751,
#14140). They used to be registry and env aliases that
`with_eval_import_scope` rolled back. A closure the `EVAL` returns keeps
the routines.

- The compiler opens the sets for the whole unit
  (`Compiler::enter_eval_unit_lexical_uses`). Their ids carry
  `EVAL_UNIT_MARK`, so a block's `use` of the same import list keeps a set
  of its own. An `EVAL`'s set is never replayed: every `EVAL` is a new
  compilation, so it re-runs the module's `sub EXPORT`.
- The ids leave out the variables section. An `EVAL`'s top-level `my` is
  not a pad of its own in mutsu, so its imported variables, constants and
  types keep the env alias.
- The `EVAL`'s undeclared-variable check now declares the variables the
  scan knows a `use`d module exports, a hook's literal keys included.
  `EVAL q[use M; $hv]` for a hook module was `X::Undeclared` before.
