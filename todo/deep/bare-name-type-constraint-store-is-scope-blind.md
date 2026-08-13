# Bare-name type-constraint store is scope-blind (residual: containers and mainline blocks)

`Interpreter::var_type_constraints` is a single global `HashMap<String,
String>` keyed by BARE variable name, and it is never frame-scoped.

## Status 2026-08-13: routine-scoped SCALARS are fixed

The main leak — a typed scalar `my` inside a routine poisoning a same-named
variable in another frame — is fixed (see
`news/2026-08/typed-lexical-constraint-frame-scoped.md`): the compiler now
emits `SetVarTypeScoped` for a scalar `my`/`state` lexically inside a
routine, which registers the constraint env-scoped only (exactly like a
typed parameter), and the return merges drop `__mutsu_type::<callee-local>`
keys. Nil-assignment reset moved into the SetLocal store path
(`typed_scalar_nil_seed_value`) so the read paths no longer need the global
map for typed routine lexicals. Pinned by
`t/typed-lexical-constraint-frame-scoped.t`; unblocked Text::CSV
`t/66_formula.t` (the last real CSV suite blocker).

## Status 2026-08-13 (later same day): genuine bare-block SCALARS are fixed

Issue 2's literal shape — a typed scalar `my` directly inside a genuine
source `{ ... }` block at mainline (compiled to `OpCode::BlockScope`) — is
now fixed the same way as the routine case: `Compiler::lexically_in_block`
(set/restored around that block's own body compilation, `stmt.rs`'s
`Stmt::Block` arm, the plain-`BlockScope` branch only) makes
`emit_set_var_type` (`compiler/mod.rs`) choose the env-only
`SetVarTypeScoped` opcode there too, and `BlockScope`'s existing env
snapshot/restore (`vm_misc_scope.rs::exec_block_scope_op`) now cleans it up
on block exit.

Fixing this exposed a SEPARATE, more general bug in that same restore: the
env-restore loop decided whether an env key was "block-local" (and so
restore-not-propagate) by checking `block_declared` (a set of *bare variable
name* symbols) directly against the key. A name-derived metadata key
(`__mutsu_type::o`, `__mutsu_hash_key_type::o`) is never itself in that set,
so a block-local `my Int $o` shadowing an outer `my Str $o` looked like an
ordinary reassignment of an outer key and propagated the block's own `Int`
constraint out, permanently overwriting the outer `Str` constraint even
after the block exited (independent of the scalar-scoping fix above — this
would affect ANY same-named-metadata-key shape, not just type constraints,
though type constraints are the only current user). Fixed by stripping the
known `__mutsu_type::`/`__mutsu_hash_key_type::` prefixes and checking the
base name against `block_declared` too.

Pinned by `t/typed-lexical-constraint-block-scoped.t` (verified against
`raku` directly, all 7 assertions match).

## What is still scope-blind

1. **`@`/`%` containers**: `my Int @a` inside a routine still registers the
   bare name in the global map (their element/key-type metadata is consulted
   through `var_type_constraint_fast` by the push/subscript/element-assign
   fast paths, which never probe env). A module method's `my Int @r` can
   still poison a caller's same-named untyped `@r` — the shape #6337 and the
   expr-position clears (`expr_block.rs`) patch case-by-case.
2. **`if`/`while`/`for`/C-style-loop BODIES with a block-local typed scalar
   still leak.** Only a genuine source `{ ... }` block was fixed above. A
   typed scalar directly in one of these bodies reaches the leak through two
   OTHER, distinct compile paths that were deliberately left untouched
   (extending the fix there needs its own VM-side work, not just the
   compiler flag):
   - **`if`/`unless`/`else` branches that declare a block-local `my`**
     compile through `Compiler::compile_block_local_branch`
     (`helpers_control_flow.rs`) → `OpCode::BlockLocalScope`, executed by
     `Interpreter::exec_block_local_scope_op` (`vm_control_ops.rs`). That
     opcode's exit cleanup only removes the bare-name env entry for a name in
     `block_declared`/`env_had_before` (`vm_control_ops.rs:326-347`) — it has
     no equivalent of `BlockScope`'s snapshot/restore, so it never touches
     `__mutsu_type::*` at all. Repro: `my $y; if True { my Str $y = "a"; };
     $y = 42` throws in mutsu, assigns fine in raku.
   - **A `while`/C-style-loop body with no topic rebind** compiles via
     `Compiler::compile_body_with_implicit_try` (`helpers_control_flow.rs`),
     which just inlines the body's statements into the current frame with NO
     scope wrapper of any kind (`Stmt::While` in `stmt.rs`). Repro: `my $x;
     while $i++ < 2 { my Str $x = "a"; }; $x = 42` throws in mutsu (once the
     loop body actually runs at least once — a body that never executes,
     e.g. `while False`, is a false negative), assigns fine in raku.
   - The likely fix shape for the `BlockLocalScope` path: snapshot the
     pre-shadow value of `__mutsu_type::<name>` (and the hash-key-type twin)
     into the SAME `Interpreter::loop_local_saved_env` map the bare name's
     shadow value already uses (`vm_var_assign_set_local.rs`'s
     `SetLocalDecl` handling, ~line 2038-2129) — that map's restore
     (`pop_loop_local_scope`) is already generic over the key string, so no
     new restore mechanism would be needed, only a second snapshot write at
     declaration time. The raw-inline `while`/loop-body path has no scope
     boundary opcode at all to hook into and would need one first.
3. **Untyped shadow destroys outer enforcement**: an untyped `my $e` /
   untyped param inside a routine REMOVES the global entry of a same-named
   outer typed lexical (the clear path in `set_var_type_constraint_impl` /
   `bind_param_type_constraint`), so after the call the outer variable loses
   enforcement through the fast paths. The env-scoped entry keeps the slow
   (env-first) paths honest.
4. **`for`-loop typed params** (`for @a -> Str $x {}`) still save/restore
   the global map by name (`vm_for_loop_body.rs`).

## The sound architecture (unchanged)

Rakudo attaches the constraint to the Scalar CONTAINER: a container created
by `my Str $e` carries `of Str` wherever it flows. The remaining fix
direction for the residuals is the same as the ticket originally proposed —
carry constraints on the container/cell (ArrayData/HashData already carry
element types; scalars would need cell-carried `of`) and make the
name-keyed store compile-time/EVAL bridging only. The container residual
(1) is the meaningful one: it needs the `_fast` consultation sites
(push/subscript) to read per-container metadata instead of the name map.
