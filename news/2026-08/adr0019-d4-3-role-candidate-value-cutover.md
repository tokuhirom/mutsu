# ADR-0019 D4-3: role-candidate resolution consumes precompiled argument values

D4-1 captured `is Parent[Args]`/`does Role[Args]`/`hides Parent[Args]` bracket content as parsed
`Expr`s; D4-2 lowered those into `CompiledClassDeclPlan::parent_arg_chunks`. D4-3 wires the class-
header composition site to actually use them: `resolve_role_candidate` gained a
`resolve_role_candidate_with_args` sibling taking `pre_args: Option<&[Value]>`, and when set, it
replaces `eval_role_arg_values`'s per-argument, per-registration re-parse-and-tree-walk of the
concatenated parent string with the already-evaluated values — everything downstream (arity
filter, trial bind, specificity sort) is unchanged. `compose_class_parent_roles` evaluates each
parent's chunk (looked up by the plan's original, pre-remap parent string, kept position-aligned
with the lexical/sibling remap chain through a zipped `Vec<Option<&[DeclTraitArg]>>`) and passes
the resulting values in.

Verified against an 8-case `raku` comparison table (literal, type name, nested parameterization,
enum value, comma-containing string, block literal) — every case matches byte-for-byte. The `Expr`
path also fixes a real, independent bug for free: `R["a,b"]` used to fail to parse at all through
the old string path's quote-blind comma splitter.

## A parser bug this cutover exposed (and fixed)

Verification surfaced a `make roast` regression in `S14-roles/parameterized-type.t`
("correct multi selected from multiple parametric roles" — `class A does R[Str] does R[Int]`).
Root cause: `parse_optional_bracket_suffix` returned an owned `String` copy of the bracket
content. Two sibling `does R[X] does R[Y]` clauses on one class header each allocate their own
short-lived copy; when the first is freed at the end of its loop iteration, the second's
allocation can land at the exact same heap address — aliasing the parser's pointer-keyed
expression memo (keyed by `(ptr, len)` with no content check) and silently returning the first
clause's cached `Expr` for the second clause's genuinely different bracket content. This bug
predates D4-3 (it lived in D4-1's new `parse_bracket_arg_exprs` call site from the start) but had
no consumer to observe it until D4-3 made `parent_arg_chunks` load-bearing.

Fixed at the root, not at the call site: `parse_optional_bracket_suffix` now returns a slice of
the *persistent* source buffer (never freed mid-parse) instead of an owned copy, so the memo can
never alias two sibling bracket clauses by construction. Pinned by a Rust unit test
(`parse_class_decl_two_does_clauses_capture_distinct_bracket_exprs`) and a `t/` integration test
(`role-double-parametric-args-distinct.t`).

## A second, unrelated bug found (and filed, not fixed)

Root-causing the above surfaced a second, independent, pre-existing bug — present on `main` before
D4-3 too, so out of scope here: composing the same parametric role twice on one class header with
different type arguments composes both multi candidates correctly, but dispatch always resolves
to whichever one wins some unidentified tiebreak, regardless of the call's actual argument type.
Filed as `todo/tickets/same-role-composed-twice-multi-dispatch-picks-one-candidate.md`.
