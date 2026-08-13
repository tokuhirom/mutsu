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

## What is still scope-blind

1. **`@`/`%` containers**: `my Int @a` inside a routine still registers the
   bare name in the global map (their element/key-type metadata is consulted
   through `var_type_constraint_fast` by the push/subscript/element-assign
   fast paths, which never probe env). A module method's `my Int @r` can
   still poison a caller's same-named untyped `@r` — the shape #6337 and the
   expr-position clears (`expr_block.rs`) patch case-by-case.
2. **Mainline blocks**: a typed scalar in a bare block at mainline
   (`{ my Str $x = "a" } ; my $x; $x = 42`) leaks within the mainline frame
   — `lexically_in_routine` is false there, so the declaration still writes
   the global map, and blocks do not restore it.
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
