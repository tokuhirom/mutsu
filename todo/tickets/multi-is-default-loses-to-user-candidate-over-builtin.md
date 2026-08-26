# A user `multi prefix:<++>` candidate wins over the builtin for `Int`/`Bool`/`Num`

Found by the doc-diff harness (`docs/doc-diff-backlog.md`, `Language/js-nutshell.rakudoc:384`).

```raku
multi prefix:<++>($a) is default { $a - 1 }
my $foo = 1;
say ++$foo;
```

- `raku`: `2`
- `mutsu`: `0`

## Root cause (re-measured 2026-08-26 — the original hypothesis was wrong)

`is default` is **not** involved. `dispatch_candidates.rs`' `is default` tie-break only runs when
two or more candidates tie (`tied.len() > 1`), and with a single user candidate it is never
reached. Removing `is default` from the repro changes nothing.

The real cause is two-layered:

1. **The dispatch decision is made at parse time, with no argument types.** When a
   `prefix:<++>` sub is in scope, `prefix_expr` (`src/parser/expr/postfix/loop_.rs:246`, via
   `match_user_declared_prefix_op` in `src/parser/stmt/simple/user_ops.rs`) rewrites `++$foo` from
   `Expr::Unary{PlusPlus}` into `Expr::Call{name: "prefix:<++>"}`. The `PreIncrement` opcode is
   never emitted, so the native increment is out of the picture entirely. Verified with
   `--dump-ast`/`--dump-bytecode`: the mainline becomes
   `WrapVarRef` + `CallFunc { name_idx: "prefix:<++>", arity: 1 }`.
2. **The native operator is not a dispatch candidate.** `resolve_function_with_types` only scans
   registered `FunctionDef`s; the native `++` is a hard-coded arm in
   `call_function_fallback` (`src/runtime/builtins_operators_fallback.rs:31-77`) reached *after* a
   failed user resolution. There is nothing for the user's `($a)` candidate to lose a narrowness
   comparison against.

In rakudo `&prefix:<++>` has ten candidates — `Mu:D`, `Mu:U`, `Int:D`, `int`, `uint`, `Bool`,
`Num:D`, `Num:U`, `num`, plus the user's `($a)`. An untyped user parameter is `Any`, which is
narrower than `Mu:D` but wider than `Int:D`.

## Exactly where mutsu diverges (measured against raku v2026.06)

With `multi prefix:<++>($a) is default { $a - 1 }` in scope:

| argument | raku picks | mutsu picks | agree? |
|---|---|---|---|
| `my $foo = 1` (Int) | builtin → `2` | user → `0` | **NO** |
| `my $b = True` (Bool) | builtin → `True` | user → `0` | **NO** |
| `my $n = 1e0` (Num) | builtin → `2` | user → `0` | **NO** |
| `my @a; ++@a[0]` (Int) | builtin → `2` | user → `0` | **NO** |
| `my $r = 1/2` (Rat) | user → `-0.5` | user → `-0.5` | yes |
| `my $s = "abc"` (Str) | user (then numeric error) | user (same error) | yes |
| `my $u` (undefined Any) | user → `-1` | user → `-1` | yes |
| `class Foo` instance | user | user | yes |

So the divergence is **only** for the types rakudo's builtin has a *typed* candidate for — `Int`,
`Bool`, `Num` (and the native `int`/`uint`/`num`). Everything else routes through the builtin's
`Mu:D` candidate, which an untyped user `Any` candidate correctly beats, and mutsu already agrees.
A user candidate that is itself typed `Int:D` is an *ambiguous call* error in rakudo; mutsu picks
the user candidate, which is a divergence not worth reproducing.

## Two more bugs in the same neighbourhood, found while measuring

- **The native `++`/`--` fallback does not mutate.** With a user `prefix:<++>` in scope that does
  *not* match, `call_function_fallback`'s `"++" => arith_add(arg, 1)` arm returns a fresh value and
  the variable is left alone: `multi prefix:<++>(Str $a) {...}; my $foo = 1; say ++$foo; say $foo`
  prints `1` / `1` where rakudo prints `2` / `2`.
- **`postfix:<++>`/`postfix:<-->` never consult a user candidate at all** (there is no parse-time
  postfix hijack and the `PostIncrement` opcode does no lookup):
  `multi postfix:<++>($a) is default { "USER" }; my $s = "abc"; say $s++` prints `abc` (native
  magic string increment) where rakudo prints `USER`. Note the *Int* postfix case agrees with
  rakudo by accident, for the opposite reason to the prefix case.

## Why this was deferred

Fixing it properly means making the native operator implementations participate in candidate
ranking, which is an architectural change, not a patch:

- The parse-time rewrite has to go (or become a flag on the increment opcodes), so that the site
  that owns the **lvalue** makes the dispatch decision. Only that site can both mutate for the
  builtin and *skip* the store-back when a user candidate wins (rakudo: a non-`is rw` user
  candidate's result is the value of `++$x` and `$x` is unchanged — confirmed for the Rat case).
- `exec_pre_increment_op_inner` (`src/vm/vm_misc_coerce.rs:311+`) and its post/decrement/index
  twins are long functions with many independent store-back branches (`ContainerRef` cell,
  slot + env flush, sigilless alias chain, atomic container, per-call anon state, package scope),
  each of which would need the "user candidate won — push and return" early exit.
- The narrowness gate itself needs the builtin's candidate types to be modelled somewhere, which
  is the general version of the problem (`infix:<+>` has the same shape: `try_user_infix` in
  `src/vm/vm_arith_ops.rs` hands *every* matching user candidate the call, so
  `multi infix:<+>($a, $b) is default { "USER" }; say 1 + 2` prints `USER` where rakudo prints `3`).

The minimal correct rule, once there is a place to apply it: **for an argument the builtin has a
typed candidate for (Int / Bool / Num, definite), a user candidate whose corresponding parameter is
untyped (`Any`/`Mu`) does not win.** That single rule reproduces every row of the table above.

## Affected files (starting point)

- `src/parser/expr/postfix/loop_.rs:246`, `src/parser/stmt/simple/user_ops.rs` — the parse-time
  prefix-op hijack
- `src/vm/vm_misc_coerce.rs` (`exec_pre_increment_op_inner`),
  `src/vm/vm_var_assign_post_incdec.rs` (post-increment, index forms) — where the lvalue lives
- `src/runtime/builtins_operators_fallback.rs:31-77` — the non-mutating native `++`/`--` arms
- `src/vm/vm_arith_ops.rs` (`try_user_infix`) — the same missing narrowness gate for infix ops
