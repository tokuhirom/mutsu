# ADR-0044: Core listops are *routines*, not a syntactic rewrite — give `push`/`pop`/`splice`/… a callable core candidate

- Status: Proposed (design complete; implementation not started)
- Date: 2026-08-20
- Related: [ADR-0019](0019-compiled-declarations-and-unified-method-dispatch.md)
  (the unified-dispatch direction this ADR is an instance of),
  [ADR-0036](0036-element-container-pairs-from-subscripts-and-pairs.md) and
  [ADR-0040](0040-array-hash-elements-are-itemized-at-the-store.md) (element
  containers — what the "autovivifying subscript first argument" case
  ultimately depends on)
- Supersedes the problem statement in
  `todo/deep/listops-are-not-real-multi-subs.md` (kept until this is
  implemented; its "imported candidate is never consulted" half is stale — see
  §2.2)

## 1. Context

Raku's array mutators — `push`, `pop`, `shift`, `unshift`, `append`,
`prepend`, `splice` — are ordinary core `multi sub`s that live in the setting.
Two consequences follow from that, and mutsu satisfies neither:

1. A user or module `multi sub push(...)` **adds a candidate** to the existing
   dispatch set. Both the core array form and the new form keep working.
2. `&push` is a real `Sub` object: `&push(@a, 1)`, `my &f = &push; f(@a, 1)`,
   `&splice.assuming(@a)` all work.

In mutsu the core listop has **no callable existence at all**. It is a
compile-time *syntactic rewrite*: `Compiler::compile_expr_call_inner`
(`src/compiler/expr_call.rs`) recognises the seven names and rewrites
`push(@a, $v)` into the method call `@a.push($v)`, emitting

```
CallMethodMut { name_idx: <"push">, arity: 1, target_name_idx: <"a">, … }
```

— an opcode that resolves its mutable target **by variable name**, at compile
time. Everything downstream (`runtime/methods_mut_dispatch.rs`,
`vm/vm_call_method_mut_ops.rs`) is method-shaped. There is no `Sub`, no
candidate, no signature, nothing multi-dispatch can rank.

## 2. The observable damage (all verified against `main` @ `b821d5e53`, 2026-08-20)

### 2.1 A local `multi` destroys the core candidate

```raku
multi splice(Str $s, Int $i) { "custom $s $i" }
my @a = (1,2,3,4,5);
splice(@a, 1, 2);          # raku: @a becomes [1 4 5]
say splice("x", 1);        # raku: custom x 1
```

raku prints `[1 4 5]` / `custom x 1`. mutsu dies on the *first* call with
`No matching candidates for proto sub: splice`. Same for `push`, `pop`, and
the no-parens statement form (`push @a, 9`).

The mechanism is all-or-nothing by construction. Two independent switches
suppress the rewrite and hand the name wholesale to user dispatch:

- `Compiler::user_listop_shadows`, seeded by
  `seed_user_listop_shadows` (`src/compiler/helpers_ast_utils.rs:397`) from the
  literal `Stmt::SubDecl`/`Stmt::ProtoDecl` list of the block being compiled;
- the parser's `make_call_expr` (`src/parser/primary/ident/listop.rs:35`),
  which emits `Expr::UserRoutineCall` instead of `Expr::Call` whenever
  `is_user_declared_sub` **or** `is_imported_function` holds for one of the
  seven names. `compile_expr_user_routine_call` then passes
  `suppress_listop_rewrite = true`.

Once suppressed, the core behaviour is unreachable: there is nothing to fall
back *to*.

### 2.2 The import case: half-fixed, still wrong (todo file is stale here)

The todo file records the imported-candidate case as `Unknown function:
splice` — i.e. the imported `multi` never being consulted at all. That half is
**fixed**: the parser's `is_imported_function` arm of `make_call_expr` now
routes the call to the imported candidate.

```raku
use MySplice;                # exports `multi splice(Str, Int, Str) is export`
say &splice.defined;         # raku: True     mutsu: True
say splice('', 0, 'Raku');   # raku: custom…  mutsu: custom…   ← now works
my @a = (1,2,3,4,5);
splice(@a, 1, 2);            # raku: [1 4 5]  mutsu: No matching candidates
```

So the failure mode has merely *moved*: it is now identical to §2.1 — the
import wins the name outright and the core array form dies. Any module that
exports a `multi` under one of these seven names silently breaks core
`push`/`splice` for the whole importing scope.

### 2.3 `&push` and friends are not callable — and fail *silently*

```raku
my @a = (1,2,3,4,5);
say &splice(@a, 1, 2);                # raku: [2 3]   mutsu: Unknown function: splice
my @b = (1,2,3); &push(@b, 7);        # raku: [1 2 3 7]
my @c = (1,2,3); my &f = &push; f(@c, 7);
```

`&splice(...)` dies with `Unknown function: splice`. Worse, `&push(@b, 7)` and
`my &f = &push; f(@c, 7)` **succeed and do nothing** — `@b` stays `[1 2 3]`.
`&push` even reports `.defined` → `True` and `.^name` → `Sub`, so the value
exists as far as introspection is concerned; it is only uncallable. A
silent wrong answer is the worst outcome in the set and is reachable without
any user `multi` at all.

### 2.4 The rewrite accretes special cases

Because the rewrite has to reconstruct, syntactically, what a real `is rw` /
raw parameter binding would give it for free, `compile_expr_call_inner` now
carries eight `suppress_listop_rewrite`-guarded branches, plus a dedicated
runtime builtin `__mutsu_push_through_accessor`
(`src/runtime/builtins_multidim_subscript.rs`) purely for
`push($obj.attr, …)`, plus an inline autovivification rewrite for
`push(@a[2], …)` / `push(%h<k>, …)`. Each new first-argument *shape* costs a
new compiler branch. A routine with a container-binding parameter costs zero.

## 3. Why the existing machinery already almost solves this

mutsu **already implements** "a user `multi` extends a core builtin" — for
every builtin that has a function-form implementation:

```raku
multi abs(Str $s) { "custom $s" }
say abs(-5);      # raku: 5        mutsu: 5
say abs("x");     # raku: custom x mutsu: custom x
```

The chain in `dispatch_func_call_inner` (`src/vm/vm_call_func_ops.rs:1310`)
is: user multi candidates are resolved first; when no candidate matches, the
call falls through `vm_call_function_fallback` → `call_function_fallback`
(`src/runtime/builtins_operators_fallback.rs`), which tries the **native
builtin function** before finally raising `X::Multi::NoMatch` at its
`has_multi_candidates(name)` guard (line 968).

For listops that fall-through hits the `X::Multi::NoMatch` guard directly,
because there is no native function-form `push`/`splice` to try. **That is the
entire gap.** The dispatch architecture is not missing; one leg of it is.

## 4. Decision

**Give each of the seven listops a real function-form core implementation,
reachable from the ordinary builtin-function dispatch chain, and demote the
compiler's syntactic rewrite to a pure fast path that is only taken when no
user/imported candidate for the name exists.**

Concretely, three commitments:

### D1 — The core listop is a callable routine, keyed on its first argument

Implement `push`/`pop`/`shift`/`unshift`/`append`/`prepend`/`splice` as native
*function*-form builtins in the `call_function_fallback` chain, taking the
first argument as the invocant and delegating to the already-correct
`methods_mut_dispatch` implementation. The invocant is the argument **value**,
not a compile-time variable name.

This is viable today without new container machinery for the dominant cases:
an `Array` argument is bound by identity, so mutating it through the function
form is already visible to the caller (verified: `sub f(@a) { @a.pop }` and
`push($s, 9)` on a `Scalar`-held array both mutate the caller's container in
mutsu, matching raku). What this leg does **not** cover is the
autovivifying-subscript first argument (`push(@a[2], …)`, `push(%h<k>, …)`)
and the accessor first argument (`push($obj.attr, …)`), because those need an
element *container* rather than a value — see D3.

D1 alone fixes §2.1, §2.2 and §2.3: it is the smallest change that makes the
core candidate survive a user/imported `multi`, and it makes `&push` real.

### D2 — The compiler rewrite becomes an optimisation, not the semantics

Keep `CallMethodMut` emission as the fast path — it is what keeps hot
`push`/`pop` free of a dispatch walk — but restate its precondition. It fires
only when **no user or imported candidate for the name is visible at the call
site**. That is what the two existing switches already approximate; the change
is that when they *do* suppress the rewrite, the call now lands on a dispatch
set that still contains the core candidate, rather than on a set that lost it.

Correspondingly, `user_listop_shadows` and the parser's `make_call_expr`
listop arm stop being a *handoff* and become a *fast-path veto*. Neither
mechanism needs to grow (the "does any candidate exist, local or imported"
question the todo file proposes is already answered by the parser's
`is_imported_function` / `is_user_declared_sub` pair); they merely stop being
load-bearing for correctness.

### D3 — Container-shaped first arguments stay on the rewrite until element containers land

`push(@a[2], …)` / `push(%h<k>, …)` / `push($obj.attr, …)` keep their existing
dedicated compiler branches for now. Making *those* work through the routine
form requires the first parameter to bind an element container with
autovivification — precisely what ADR-0036 (subscript-produced element
containers) and ADR-0040 (itemization at the store) are for. This ADR
deliberately does **not** block on them: the extension cases that are actually
broken in the wild are the plain `@array` / `$scalar-held-array` ones.

The consequence to accept explicitly: until ADR-0036/0040 land, a user `multi
push` in scope makes `push(@a[2], 1)` fall to the routine form, which will not
autovivify. That combination (user `multi push` *and* an autovivifying
subscript listop in the same scope) is vanishingly rare, and it fails loudly
rather than silently. Record it in the ADR's implementation status rather than
building a third mechanism for it.

## 5. Alternatives considered

**A. "Try user candidates, else fall back to the rewrite" as a compiler
branch.** Emit both paths and pick at runtime. Rejected: it duplicates the
dispatch decision in the compiler, cannot see candidates registered after
compile time (a module loaded by a later `use`, `EVAL`, `augment`), and adds a
ninth special case to a function that already has eight. It also leaves
§2.3 (`&push` as a value) completely unfixed — a `Sub` object cannot be
synthesised from a compiler rewrite.

**B. Register the seven names as `proto sub` + native candidates in the
runtime registry, so ordinary multi ranking sees them.** This is the
"maximally correct" reading and is where §6 eventually goes: it would make
narrowness ranking between a core `(Positional, Int)` candidate and a user
`(Any, Int)` candidate come out right. Rejected *for now* as the first move
because the core candidates would need real signature objects with raw
container parameters to rank against, which is D3's dependency; and because
the fall-through ordering in D1 gets the same answer for every case anyone has
actually hit. Adopting B later is a strict refinement of D1 — the native
function bodies written for D1 become the candidate bodies — so D1 does not
foreclose it.

**C. Do nothing; document the limitation.** Rejected: §2.3's silent no-op is a
wrong-answer bug with no user `multi` involved, and §2.2 means any ecosystem
module exporting one of these seven names quietly breaks core array mutation
for its importers.

## 6. Consequences

- **Fixed:** local `multi` extension (§2.1), imported `multi` extension
  (§2.2), `&push`/`&splice` as callable values including the silent-no-op
  (§2.3).
- **Not fixed by this ADR:** narrowness ranking between core and user
  candidates (alternative B); autovivifying/accessor first arguments under a
  user `multi` (D3).
- **Performance:** unchanged on the hot path. D2 keeps `CallMethodMut` as the
  emitted form for every call site with no competing candidate — which is
  every call site in the roast suite and in every bundled battery today.
- **Debt retired:** the `__mutsu_push_through_accessor` builtin and the
  subscript-autoviv rewrite become removable once D3's dependency lands,
  because a routine parameter binding an element container subsumes both.

## 7. Verification plan

New `t/` pins, each written to fail on `main` today:

1. `t/listop-multi-extends-core.t` — local `multi splice`/`multi push`/`multi
   pop` alongside the core array form, both parenthesised and no-parens
   statement forms.
2. `t/listop-imported-multi-extends-core.t` — a `lib/` fixture module
   exporting `multi splice is export` **without** exporting a `proto`; assert
   both the imported and the core array form dispatch.
3. `t/listop-as-code-value.t` — `&push(@a, 1)`, `my &f = &splice; f(@a, 1, 2)`,
   `&pop(@a)`, asserting the *mutation*, not just the return value (the
   current bug is a silent no-op, so a return-value-only assertion would pass
   vacuously).

Plus a `--dump-bytecode` assertion (or a `#[test]`) that a plain
`push(@a, 1)` with no competing candidate still compiles to `CallMethodMut`,
pinning D2's fast path against accidental regression to the slow form.

## 8. Implementation status

Not started. D1 is self-contained and is the correct first slice; D2 is a
one-line precondition restatement that must land with it; D3 is a recorded
non-goal pending ADR-0036/ADR-0040.
