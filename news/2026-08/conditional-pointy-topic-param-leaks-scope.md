# A `$_` pointy parameter on a conditional now gets a real topic scope instead of an ordinary lexical declaration

`$_ = 1; if 5 -> $_ { say $_ }; say $_` printed `5` then `5` under mutsu where
`raku` prints `5` then `1`. The same leak affected `unless`, `elsif`, the `else`
form, and — found while fixing it — every `with`/`without` pointy-topic form as
well.

## Root cause

`lower_if_clause_binding` / `lower_else_binding`
(`src/parser/stmt/control/conditionals.rs`) lowered a single simple pointy
parameter to a `binding_var` on the `Stmt::If` (or to a `Stmt::VarDecl`
prepended to the else body). For an ordinary name that declares a block-scoped
lexical; for `$_` it declares the *topic*.

`if EXPR -> $_` did already emit an `EnterPointyTopic` / `ExitPointyTopic` pair,
and `ExitPointyTopic` restores `env["_"]` — so the diagnosis "the topic is never
restored" was only half right. The bytecode shows why it did not help:

```
3: EnterPointyTopic
6: SetLocalDecl { slot: 0, explicit_init: false }   # `my $_ = 5`
…
12: ExitPointyTopic                                  # restores env["_"] only
13: GetLocal(0)                                      # `say $_` reads the SLOT
```

The declaration allocates a local slot for `_`, every later read of `$_` in the
frame compiles to `GetLocal` on that slot, and `ExitPointyTopic` restores only
the env mirror. `with COND -> $_` had no topic op at all — it pushed a plain
`Stmt::VarDecl` for the parameter — and left `$_` as `Nil` afterwards.

## Fix

Route a `$_` conditional binding through `given`, exactly as the `orwith` arm
already does (and for the same reason its comment gives): `given`'s own topic
opcodes establish and restore the scope, slot included.

- `if COND -> $_ { BODY }` now lowers to a hidden `$__mutsu_if_bind_N` binding
  (so the condition is still evaluated exactly once and still drives the
  branch test) with the body wrapped in `Stmt::Given { topic: $__mutsu_if_bind_N }`.
  `unless` and `elsif` share this lowering; `else -> $_` gets the same treatment
  against the chain's existing source binding.
- `src/parser/stmt/control/with_stmt.rs` grew a `pointy_is_topic` flag: a `-> $_`
  parameter suppresses the `VarDecl` bind and routes the body through
  `given $tmp` (or, for an lvalue condition, through the existing
  `use_given_alias` path with the `pointy_topic_bind` insert suppressed —
  `given` already installs the topic). The `else -> $_` arm of a `with` chain
  likewise skips its `VarDecl`, since its body is already inside a `given`.

Named pointy parameters (`-> $v`), sigilless (`-> \r`), attributive (`-> $!x`),
container (`-> @p`) and sub-signature parameters are untouched.

Pinned by `t/itemization-and-readonly.t`, which covers `if` / `else` / `unless` /
`elsif` / `with LITERAL` / `with VAR` / `with … else` / an untaken branch, and
passes under real `raku`.
