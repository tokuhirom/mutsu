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
