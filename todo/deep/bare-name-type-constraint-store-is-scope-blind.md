# Bare-name type-constraint store is scope-blind (residual: containers and mainline blocks)

`Interpreter::var_type_constraints` is a single global `HashMap<String,
String>` keyed by BARE variable name, and it is never frame-scoped.

## Spot-check 2026-08-27 (`main` @ `10ac4d450`): re-measure before dispatching slice 2

Five rows this ticket and ADR-0042 §3 use as motivation now **agree with
`raku`**, including the one ADR-0042 §5.2 names as the reason slice 2 is "the
architectural half":

| row | program | mutsu | raku |
| --- | --- | --- | --- |
| A | `my Str $s; my $t := $s; $t = 42` — **ADR-0042 §3's headline** | dies, correct message | dies |
| B | `my $y; if True { my Str $y = "a" }; $y = 42` (residual 2, `if` branch) | `42` | `42` |
| C | `my $x; my $i = 0; while $i++ < 2 { my Str $x = "a" }; $x = 42` (residual 2, `while` body) | `42` | `42` |
| E | `my Str $s = "a"; my \x := $s; x = 42` | dies | dies |
| F | `my Str $e = "a"; sub f { my $e = 1 }; f(); $e = 42` (residual 3) | dies | dies |

Row A dying correctly means the *correctness* argument for giving the scalar
cell an `of` field has, at least for this shape, already been satisfied by some
other change. Rows B and C are residual 2's two literal repros, quoted verbatim
from this file's own "What is still scope-blind" section; both now agree.

**This is a spot-check, not a closure.** Only five rows were run — ADR-0042's
§2.1 and §2.2 matrices are larger, and CLAUDE.md's standing lesson is that a
*partial* fix is more common than full staleness, so a headline repro passing
is exactly when to re-run the whole table rather than assume. Slice 2 also has
a second, independent motivation this measurement says nothing about: deleting
the `box_decl_local_cell` constraint bails and unblocking slice 3's removal of
the global map and its six workarounds.

**Before dispatching ADR-0042 slice 2, re-run §2.1 and §2.2 in full and rewrite
its §3 with what actually still diverges.** Dispatching it on the ADR's stated
motivation as written would be work against a repro that no longer fails.

## Status 2026-08-23: ADR-0042 Slice 1 is SHIPPED — residual 1 is closed

Slice 1 landed 2026-08-20 as PR #6743 (`dc39cb3e3`), and the follow-on
"outer-first shadow" finding landed 2026-08-22 as `b388b1b9f`
(`news/2026-08/typed-declaration-shadow-scope-leak.md`). Re-verified on
`c10d305d4`, `raku`-oracled: the ADR's §2.2 container matrix matches `raku`
7/7, the §3 alias probe (enforcement reached through a differently-named bound
alias, which only the container can supply) matches 8/8, and the §3.1 `state`
container gap is closed. Pins green: `t/typed-constraint-scope-matrix.t`,
`t/state-typed-container-alias.t`, `t/typed-constraint-shadow-scope.t`,
`t/typed-lexical-constraint-frame-scoped.t`,
`t/typed-lexical-constraint-block-scoped.t`.

**Residual 1 below (`@`/`%` containers) is therefore CLOSED** — a container's
element/key constraint is now read from the value's own embedded
`ArrayData`/`HashData` metadata at the ten mutation chokepoints, not from the
bare-name map. Residual 4 was already found not to reproduce (ADR-0042 §9).

This ticket stays open for **ADR-0042 Slices 2 and 3**, which are NOT started:
slice 2 (a scalar cell carries its `of`, the architectural half — residuals 2
and 3 below) and slice 3 (delete the map and its workarounds). Do not re-dispatch
slice 1.

## Status 2026-08-20: superseded by ADR-0042 — read that first

The remaining work is now designed in
[ADR-0042](../../docs/adr/0042-type-constraints-belong-to-the-container-not-to-a-name.md).
Re-verified against `3766df1de`; the "What is still scope-blind" section below
is **stale in two ways** and the ADR carries the corrected, measured version:

- **The scalar residual is larger than recorded.** Seven shapes still leak, not
  the two compile paths issue 2 names: `if`, `unless`, `else`, `while`,
  C-style `loop`, `repeat`, and `for` bodies. (Loop bodies take
  `compile_body_with_implicit_try`, which emits *no* scope wrapper at all.)
- **The container residual is the opposite of "the meaningful one".** `ArrayData`
  and `HashData` already carry `value_type`/`key_type`, and a differently-named
  bound alias enforces correctly in 8 of 8 container shapes — so for containers
  the name map is a redundant second source of truth contributing only false
  positives, and the container-first accessor (`element_constraint_for`) already
  exists. That half is mechanical. The genuinely architectural half is
  **scalars**: `ContainerRef(Gc<Mutex<Value>>)` has no constraint field, so
  `my Str $s; my $t := $s; $t = 42` wrongly succeeds.

Residual 4 (`for`-loop typed params) does not reproduce as a divergence; see
ADR-0042 §9. Slice 1 of ADR-0042 was ready for direct implementation — it was
mechanical and needed no further design. **It has since shipped; see the
2026-08-23 status above.**

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

## What is still scope-blind (as recorded 2026-08-13 — see the stale-scope note above)

1. **[CLOSED 2026-08-23 by ADR-0042 slice 1, PR #6743.]** **`@`/`%` containers**:
   `my Int @a` inside a routine still registers the
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
by `my Str $e` carries `of Str` wherever it flows. The fix direction is to
carry constraints on the container/cell and make the name-keyed store
compile-time/EVAL bridging only.

**Correction (2026-08-20):** the sentence that used to end this section —
"the container residual (1) is the meaningful one" — had the two halves
backwards. Containers already carry their constraint on `ArrayData`/`HashData`,
so residual 1 reduces to routing the `_fast` consultation sites through the
existing `element_constraint_for` accessor: mechanical, not architectural.
Scalars are the architectural half, because the scalar cell has no `of` field
at all. ADR-0042 §3 has the measurements.
