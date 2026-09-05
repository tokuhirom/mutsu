# A method's rw-capability is one oracle, not two rules

ADR-0067 slice 2. mutsu asked two different questions about the same
declaration property depending on whether the routine was spelled `sub` or
`method`. For a `sub`, `Interpreter::routine_is_rw_capable` has long stated the
Rakudo rule — `is rw`, `is raw`, or an explicit `return-rw` anywhere in the body
all make a routine hand its caller a *container*. The method lvalue path tested
`is_rw` alone, and `MethodDef` had no `is_raw` field at all because
`Stmt::MethodDecl` never carried one (`SubDecl` carries both). The parser was
already computing `traits.is_raw` for a method and throwing it away.

The measured consequence, against `raku` v2026.07:

| Program | raku | mutsu (before) |
|---|---|---|
| `class C { method m(\x) is rw { x } }; C.new.m($a) = 5` | `5` | `5` |
| the same with `is raw` | `5` | `X::Assignment::RO: method 'm' is not rw` |
| `method m(\x) { return-rw x }` | `5` | the same error |
| `C.m($a) = 5` (type object) with `is raw` | `5` | `42` — **silent**, exit 0 |

`is_raw` is now carried through `Stmt::MethodDecl` (`src/ast.rs`),
`CompiledMethodDecl` (`src/opcode.rs`) and `MethodDef`
(`src/runtime/decl_types.rs`), and the new
`Interpreter::method_is_rw_capable` (`src/runtime/builtins_lvalue.rs`) states
the rule once for methods, exactly as `routine_is_rw_capable` does for subs. It
backs all three method gates: the unqualified and `Class::method` refusals in
`methods_mut_method_lvalue.rs`, and `method_lvalue_returns_container` in
`lvalue_container_return.rs` — the last of which is also what stops the legacy
`$obj.name($value)` setter convention from pre-empting the lvalue return. Two
`FunctionDef` sites that build the sub-form of an `our method` / `my method`
were dropping `is_raw` on the floor as well (`is_raw: false` beside
`is_rw: decl.is_rw`); they propagate it now.

## The runtime gate was only half the fix

Routing the runtime gate through the oracle made `return-rw` work and left
`is raw` failing, with a *different* error: `rw method 'm' does not expose an
assignable attribute`. The gate now admitted the call, but the method body's
tail had been compiled as an ordinary value read, so there was no container for
the assignment to write through.

The compile side keys the rw tail off the same declaration and had the same
`is_rw`-only narrowness, in two places that must agree: the main-pass
`compile_method_body` call in `src/compiler/decl_plan.rs`, and the
registration-time `compile_method_def_in_place_with_dist` in
`src/runtime/accessors_resolve.rs`. Both now pass `is_rw || is_raw`, mirroring
what `compile_sub_body` has always done (`sub_compiler.rw_tail = is_rw ||
is_raw`). The generalisable lesson, recorded in the ADR for slices 3-5: a
capability gated at runtime is usually also gated at compile time, and the two
halves have to move together — a green runtime gate on its own just relocates
the error message.

## Pin

`t/method-rw-capability-oracle.t`, 22 tests with byte-identical output under
`mutsu` and `raku`: the three rw-capable spellings over both instance and
type-object invocants, over scalar / array-element / hash-element containers,
and through `multi`, role composition and `augment`; the non-rw-capable
regression controls (a plain method must still refuse, and must not write the
caller's variable); and the `is rw` attribute-accessor shapes — `has $.v is rw`,
`has @.items` element writes — that name a location rather than computing one
and that the widened oracle must not have disturbed.

## Found while measuring, not fixed here

For a **type-object** invocant whose method is *not* rw-capable, mutsu still
reports success and silently drops the write where raku dies, because the legacy
setter convention catches it — and with a sigilless parameter it calls the
method with the *invocant* as its argument. The instance twin already refuses
correctly. That is a different mechanism with no declaration-level oracle to
gate on, so it is recorded as
`todo/tickets/type-object-lvalue-falls-into-setter-convention.md` and the pin
asserts the weaker fact that currently holds (the caller variable is untouched).
