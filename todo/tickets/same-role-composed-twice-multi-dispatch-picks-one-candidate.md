# Composing the same parametric role twice with different type args loses multi dispatch by argument type

`class A does R[Int] does R[Str]` where `role R[::T] { multi method foo(T $t) {...} }`
correctly composes BOTH multi candidates into `A` (confirmed via `.^methods(:local)` and via the
roast test `S14-roles/parameterized-type.t`'s "correct multi selected from multiple parametric
roles" subtest, which only exercises the `Int` call and passes). But calling `.foo` with an
argument that should select the *other* candidate dispatches to the wrong one — reproducible on
`main`, independent of ADR-0019 D4:

```
my role R[::T] { multi method foo(T $t) { "T=" ~ T.^name } };
my class A does R[Int] does R[Str] { };
say A.new.foo(5);    # mutsu: "T=Str" (wrong, should be "T=Int")
say A.new.foo("x");  # mutsu: "T=Str" (correct, but only by accident)
```

raku prints `T=Int` then `T=Str` — correct multi dispatch by argument type. mutsu always
dispatches both calls to whichever candidate's substituted `MethodDef` happens to win a — as yet
unidentified — resolution/cache step; swapping the `does` order flips which candidate wins,
suggesting a last/first-registered tiebreak rather than genuine per-call signature matching.

## Where this was found

Discovered 2026-08-08 while implementing ADR-0019 D4-3 (`resolve_role_candidate_with_args`,
`todo/deep/adr0019-d4-parent-expr-chunks.md`) and root-causing why `S14-roles/parameterized-type.t`
started failing on the D4-3 branch: D4-3 exposed a *different*, real bug (see the fix landed in
D4-3's PR: `parse_optional_bracket_suffix` returning an owned `String` let the pointer-keyed
expression parse memo alias two sibling bracket arguments), but investigating it surfaced this
independent, pre-existing multi-dispatch bug — confirmed present on `main` before D4-3 by running
the repro above against a pre-D4-3 build.

## Why this is filed rather than fixed here

Root-causing requires tracing the multi-candidate resolution/caching path
(`multi_resolve_cache`/`dispatch_multi_candidate` in `vm.rs`, or the method composition dedup in
`registration_class_compose.rs`) to find why two structurally-different substituted `MethodDef`s
for the same `(class, method name)` don't both survive dispatch — out of scope for a
declaration-plan-lowering slice. The one roast test that exercises this shape only calls with one
argument type, so it doesn't currently catch the bug; a fix should add a two-argument-type
regression test (`t/role-double-parametric-multi-dispatch.t`) alongside the real fix.

## Deep-dive investigation (2026-08-10)

### Confirmed root cause: multi dispatch is CORRECT — the shared per-class `T` binding is wrong

Both suspects named above are exonerated. The bug is not in candidate storage, not in a
composition dedup, and not in `multi_resolve_cache`/`dispatch_multi_candidate`. **Both
substituted `MethodDef`s survive into dispatch with correctly substituted signatures, and the
multi-dispatcher selects the correct candidate per argument type.** What is wrong is the value
the selected candidate's *body* sees for the bareword type `T`: it is injected into the method
env at call time from `Registry::class_role_param_bindings` — a flat
`class -> (param name -> value)` map (`src/runtime/registry.rs:241`) — and the **second
composition of the same role overwrites the first composition's `T` entry**, because the map is
keyed only by the bare param name `"T"`, not by which parameterization the executing candidate
came from.

The exact overwrite site is `src/runtime/registration_class_compose.rs:219-223`
(`compose_role_into_class`): each composition does
`cx.out.class_role_param_bindings.insert(p.clone(), v.clone())` into a single flat
`FxHashMap<String, Value>` shared by all compositions of the class
(`RoleCompositionOutcome.class_role_param_bindings`, same file line 61). For
`does R[Int] does R[Str]` the first composition inserts `T => Int`, the second inserts
`T => Str` over it. The merged flat map is then stored per class at
`src/runtime/registration_class_decl.rs:200-206`. At method-call time,
`call_compiled_method` injects it into the body env at
`src/vm/vm_method_dispatch.rs:335-344` ("Role param bindings":
`self.class_role_param_bindings(owner_class)` then a name-keyed `env_mut().insert`), so BOTH
candidates' bodies read `T == Str`.

### Evidence

1. **Candidates + substituted signatures survive.** Composition appends, never overwrites:
   `registration_class_compose.rs:315-357` builds `composed` via
   `substitute_type_params_in_method` (`src/runtime/registration_class.rs:298-367`, which
   correctly rewrites each candidate's `type_constraint` `T -> Int` / `T -> Str`) and does
   `cx.class_def.methods.entry(mname).or_default().extend(composed)`. Calling with a
   non-matching arg proves both are consulted with correct types:
   `A.new.foo(3.5)` dies with `none of these signatures matches: (A: Int $t, ...) (A: Str $t, ...)`.
2. **Selection is per-argument-type correct.** Changing the body to not mention `T`
   (`multi method foo(T $t) { "arg=" ~ $t.^name }`) prints `arg=Int` / `arg=Str` — the right
   candidate runs for each call.
3. **gdb proof that the injection site is the mechanism.** With a breakpoint on
   `src/vm/vm_method_dispatch.rs:338` (the binding-injection loop inside
   `call_compiled_method`), the repro hits it on both calls with `owner_class="A"`, and the two
   calls carry **different `method_def` pointers** (`0x7fffe00870a0` for `foo(5)`,
   `0x7fffe00928d0` for `foo("x")`) — i.e. dispatch selected two distinct candidates — yet both
   print `T=Str`, because the injected map is the class-level `{T => Str}`. Backtrace:
   `exec_call_method_op_impl -> try_compiled_method_or_interpret_* ->
   dispatch_compiled_method (vm_call_method_compiled_cache.rs:348) -> call_compiled_method`.
4. **Registration-time, not call-order caching.** Calling with the Str arg first changes
   nothing (`T=Str` / `T=Str`); swapping the `does` order flips both outputs to `T=Int` —
   exactly the flat-map last-write-wins signature. `multi_resolve_cache` keys include argument
   types and returns the correct per-type candidate (point 3), so the caches need no change and
   no invalidation.

Related but SEPARATE cosmetic discrepancy noticed en route: `A.^methods(:local)` on current
main lists only ONE `foo` (with the Int signature) while raku lists one proto-like entry with
`.candidates.elems == 2`. That is introspection name-dedup, not this bug (the ticket header's
".^methods shows both" observation is stale), and the fix below does not address it.

### Variant behavior table (current main, debug build 2026-08-10)

| Variant | mutsu | raku | Verdict |
|---|---|---|---|
| `does R[Int] does R[Str]`; `foo(5)`, `foo("x")` | `T=Str`, `T=Str` | `T=Int`, `T=Str` | the bug |
| `does R[Str] does R[Int]` (swapped) | `T=Int`, `T=Int` | `T=Int`, `T=Str` | last `does` wins — registration-time overwrite |
| Call `foo("x")` before `foo(5)` | `T=Str`, `T=Str` | `T=Str`, `T=Int` | call order irrelevant — NOT a first-call cache |
| `foo(3.5)` (matches neither) | dies listing BOTH `(A: Int $t ...)` and `(A: Str $t ...)` | dies listing both | both candidates + substituted types survive dispatch |
| Body `"arg=" ~ $t.^name` (no `T` read) | `arg=Int`, `arg=Str` | same | candidate SELECTION is correct |
| plus class-body `multi method foo(Rat $t)` | `class-Rat` for `foo(3.5)`; `T=Str`/`T=Str` for Int/Str args | `class-Rat`; `T=Int`/`T=Str` | class multi coexists; role-candidate `T` still wrong |
| `also does R[Int]; also does R[Str]` in class body | dies: `Cannot resolve caller foo(A:D: Int); ... (A: T $t, ...)` — UNsubstituted `T` | `T=Int`, `T=Str` | DIFFERENT pre-existing bug: `todo/tickets/also-does-role-bracket-args-dropped-in-class-body.md`. Do not test `also does` in this fix |
| Single composition `does R[Int]` (two classes, one arg type each) | correct | correct | per-class map is fine when each class composes the role once |

### Fix plan (step by step)

The candidate already knows which parameterization it came from at composition time; the fix is
to stamp the bindings **per `MethodDef`** and prefer them over the per-class map at injection
time. Keep `class_role_param_bindings` itself — its other consumers (constructor/attr-default
eval at `src/runtime/methods_object_dispatch_new.rs:1630-1646` and `:1869-1887`, role-pun
construction `:557-584`, qualified concretization calls in `src/runtime/methods_qualified.rs`,
EVAL snapshotting in `src/runtime/system_eval_string.rs`, `t`est-function nested interpreters)
are per-class by nature (composing two parameterizations that both declare `has T $.x` is an
attribute conflict in raku anyway).

1. **Add the field** to `MethodDef` (`src/runtime/decl_types.rs:95-140`):

   ```rust
   /// Role type-parameter bindings for THIS composed candidate (`T => Int`),
   /// stamped by `compose_role_into_class` when a parameterized role is
   /// composed. Injected into the body env at dispatch in preference to the
   /// per-class `class_role_param_bindings` map, which is last-write-wins
   /// when the same role is composed twice with different args. `None` for
   /// methods not composed from a parameterized role.
   pub(crate) role_param_bindings: Option<std::sync::Arc<Vec<(String, Value)>>>,
   ```

   `Arc` keeps `MethodDef::clone` O(1). There are 15 struct-literal construction sites (no
   `Default`), in: `src/runtime/system.rs`, `registration_class_body_method.rs`,
   `dispatch_proto.rs`, `methods_classhow_dispatch.rs`, `decl_types.rs`,
   `registration_role_method.rs`, `registry.rs`, `runtime_init.rs`, `registration_class.rs`,
   `registration_class_augment.rs`. Add `role_param_bindings: None` to each EXCEPT
   `substitute_type_params_in_method`'s rebuild (`registration_class.rs:347-366`), which must
   carry it through: `role_param_bindings: method.role_param_bindings.clone()`. The compiler
   enforces completeness — do not add `..Default::default()`.

2. **Stamp at composition** in `compose_role_into_class`
   (`src/runtime/registration_class_compose.rs:315-357`). Before the `let composed = ...`
   block, build once per role application:

   ```rust
   let candidate_bindings: Option<std::sync::Arc<Vec<(String, Value)>>> =
       if role_param_names.is_empty() {
           None
       } else {
           Some(std::sync::Arc::new(
               role_param_names
                   .iter()
                   .cloned()
                   .zip(role_arg_values.iter().cloned())
                   .collect(),
           ))
       };
   ```

   and in BOTH map closures (the `type_subs.is_empty()` branch and the substituting branch)
   set `method.role_param_bindings = candidate_bindings.clone();` next to the existing
   `role_origin` stamping. (Both branches: `type_subs` pairs names with `type_value_name`
   strings and is empty iff `role_param_names` is empty, but stamping via
   `candidate_bindings` keeps the two branches symmetric and covers value-typed role params.)

3. **Prefer per-candidate bindings at injection**, `src/vm/vm_method_dispatch.rs:335-344`
   (`call_compiled_method`), keeping the block exactly where it is (BEFORE the
   `captured_env` merge at line 351 and before param binding, to preserve precedence):

   ```rust
   // Role param bindings — prefer the candidate's own composition bindings
   // (same role composed twice with different args gives each candidate its
   // own T); fall back to the per-class map for class-body methods.
   if let Some(bindings) = method_def.role_param_bindings.as_deref() {
       for (name, value) in bindings {
           self.env_mut().insert(name.clone(), value.clone());
       }
   } else if let Some(role_bindings) = self.class_role_param_bindings(owner_class) {
       ...existing two arms unchanged...
   ```

4. **Defensive gate updates** (today the class map is always non-empty whenever any candidate
   carries bindings, but `registration_class_validate.rs:35-67` removes/restores the class map
   on re-registration paths, so make the gates self-sufficient):
   - `src/vm/vm_method_dispatch.rs:123-126`: `has_role_bindings` gains
     `|| method_def.role_param_bindings.is_some()`.
   - `src/vm/vm_call_method_compiled_cache.rs:428-434`: same addition to its
     `has_role_bindings` (this gate routes binding-carrying methods away from the pinned fast
     path; it must keep doing so).

5. **Candidate matching** (`src/runtime/resolution_method.rs:14-75`,
   `method_args_match_for_invocant`): it injects the class-level `role_bindings` parameter
   into the env before matching. Since each candidate's `type_constraint` is already
   substituted, this only matters for a `where` clause or sub-signature that reads `T`
   directly. Inside the function, prefer `def.role_param_bindings` (the `def: &MethodDef`
   parameter is already in scope) over the passed-in class-level map; no caller-signature
   changes needed (callers: `resolution_method.rs:189/:613`,
   `resolution_private_method.rs:40/137/151`, `resolution_sequence.rs:150`).

6. **Do NOT touch**: `multi_resolve_cache` / `func_multi_resolve_cache` /
   `dispatch_multi_candidate` (they key on argument types and already resolve correctly; the
   fix changes only the env the resolved candidate's body runs with, which is not cached);
   the constructor/attr-default consumers listed above; `roast/` anything.

### Test plan

Add `t/role-double-parametric-multi-dispatch.t` with exactly this content (verified 12/12 OK
under `raku` on 2026-08-10; on current mutsu main tests 1, 3, 6, 9 fail with `T=Str`/`T=Int`
inversions, everything else already passes):

```raku
use v6;
use Test;

plan 12;

my role R[::T] { multi method foo(T $t) { "T=" ~ T.^name } }

# Same parametric role composed twice with different type args:
# each candidate's body must see ITS OWN T binding.
my class A does R[Int] does R[Str] { }
is A.new.foo(5),   "T=Int", "Int arg selects the R[Int] candidate and binds T=Int";
is A.new.foo("x"), "T=Str", "Str arg selects the R[Str] candidate and binds T=Str";

# Swapped composition order must not change the outcome.
my class B does R[Str] does R[Int] { }
is B.new.foo(5),   "T=Int", "swapped does order: Int arg still binds T=Int";
is B.new.foo("x"), "T=Str", "swapped does order: Str arg still binds T=Str";

# Call order must not matter either (no first-call cache poisoning).
my class C does R[Int] does R[Str] { }
is C.new.foo("x"), "T=Str", "Str-typed first call binds T=Str";
is C.new.foo(5),   "T=Int", "Int-typed second call binds T=Int";

# An argument matching neither candidate dies (both candidates must
# survive into dispatch with correctly substituted signatures).
dies-ok { A.new.foo(3.5) }, "Rat arg matches neither Int nor Str candidate";

# A class-body multi of the same name coexists with the role candidates.
my class D does R[Int] does R[Str] { multi method foo(Rat $t) { "class-Rat" } }
is D.new.foo(3.5), "class-Rat", "class-body Rat candidate wins for Rat";
is D.new.foo(5),   "T=Int",     "role Int candidate still selected alongside class multi";
is D.new.foo("x"), "T=Str",     "role Str candidate still selected alongside class multi";

# Single composition (the already-working shape) keeps working.
my class E does R[Int] { }
is E.new.foo(5), "T=Int", "single composition binds T=Int";
dies-ok { E.new.foo("x") }, "single composition rejects a Str arg";
```

Verification sequence: `cargo build`, `prove -e target/debug/mutsu
t/role-double-parametric-multi-dispatch.t`, then the targeted regression set below, then
`cargo fmt` + `cargo clippy -- -D warnings` + `make test`, and let CI run the full roast.

### Pitfalls / regression hazards

- **Targeted regression tests** (run locally before pushing; all exercise
  parameterized-role method composition / role-param env injection):
  - `MUTSU_FUDGE=1 prove -e 'target/debug/mutsu' roast/S14-roles/parameterized-type.t`
    (whitelisted; contains the "correct multi selected from multiple parametric roles"
    subtest — the one-arg-type shape this ticket generalizes)
  - `MUTSU_FUDGE=1 prove -e 'target/debug/mutsu' roast/S14-roles/parameterized-basic.t`,
    `roast/S14-roles/parameter-subtyping.t`, `roast/S14-roles/generic-subtyping.t`,
    `roast/S14-roles/composition.t` (all whitelisted)
  - local: `prove -e target/debug/mutsu t/qualified-role-multi-concretization.t
    t/role-samename-composition-and-nested-class.t t/parametric-role-typed-var.t
    t/role-parameterisation-keeps-the-topic.t t/my-enum-and-type-params-in-role.t`
- **Precedence inside `call_compiled_method`:** the binding injection must stay exactly where
  the current block is — after `self`/`?CLASS` setup, BEFORE the `captured_env`
  insert-if-absent merge (`vm_method_dispatch.rs:351-358`) and before parameter binding, so
  params/self still shadow `T` and captured envs do not shadow the fresh binding.
- **`substitute_type_params_in_method` rebuilds `MethodDef` field-by-field** — forgetting the
  carry-through there silently reverts the fix for exactly the substituted candidates it is
  meant for (the compiler catches a *missing* field, and copying `None` would also compile —
  copy `method.role_param_bindings.clone()`).
- **Runtime mixins** (`$obj does R[Int]`, `but`) build their bindings separately
  (`methods_qualified.rs:664-690`, per-value) and likely have the same last-wins shape when
  the same role is mixed twice; out of scope here (see
  `todo/tickets/mixin-role-order-not-tracked.md`), but do not regress that path — it does not
  read `MethodDef.role_param_bindings`, so the fix is orthogonal to it.
- **`also does` stays broken by a different bug** (bracket args dropped, signature keeps
  unsubstituted `T` — see the variant table). Keep it out of the new test file.

### ADR-0019 interaction

No conflict; the fix is aligned with the ADR's own plan and should land now, in the legacy
code, as described above.

- The buggy consumption site (`call_compiled_method`, `vm_method_dispatch.rs:335`) and the
  stamping site (`compose_role_into_class`) are legacy machinery that ADR-0019 will eventually
  rework, but no Phase E resolver code has landed for candidate execution (E8-E11 are
  design-only as of 2026-08-09, `todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md`),
  and Phase D's role-plan encoding (D7, landed) still materializes composed candidates as
  `MethodDef`s — so `MethodDef.role_param_bindings` is a carrier that survives both phases.
- The ADR explicitly anticipates this fix: `docs/adr/0019-...md:844-845` notes "role method
  bodies may need **per-composition re-instantiation** depending on how a parametric role's
  type captures reach compiled bytecode" (D3-8 scoping). Per-candidate bindings stamped at
  composition IS that per-composition instantiation, done as data instead of recompilation.
- Coordination hazard to note in the PR: the E4/E8 "ordered candidate sequence" resolver, when
  it arrives, must inject `role_param_bindings` from the *selected* candidate (not from a
  class-level map) in whatever replaces `call_compiled_method`'s env setup — the new field
  makes that a one-line rule. Also `drop_flattened_role_duplicates`
  (`resolution_method.rs:613-630`) de-dups candidates at resolution time because the composed
  role is also an MRO entry; it compares candidates, and after this fix two same-name
  candidates differ in `role_param_bindings` as well as `param_defs` — it must keep treating
  the two substituted candidates as distinct (they already differ in `param_defs`, so current
  behavior is safe; just do not "simplify" it to compare by role_origin+name).
