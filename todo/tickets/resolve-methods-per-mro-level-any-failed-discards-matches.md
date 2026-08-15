# `resolve_methods_per_mro_level`'s `any_failed` gate discards every successful `.*`/`.+` match

## What's wrong

`Interpreter::resolve_methods_per_mro_level` (`src/runtime/resolution_method.rs:742`), the winner
list builder behind `.*name`/`.+name` (all-candidates dispatch, used e.g. by
`call_method_all_with_values`), collects every MRO level that defines the method into
`defining_levels`, then — for a multi method — resolves each defining level independently via
`resolve_method_with_owner_impl(cn, method_name, arg_values, None, None)`, using that level's own
name as a fresh MRO-walk start. If **any single level** fails to resolve a matching candidate for
the given call's arguments, the function sets `any_failed = true` and a few lines later does:

```rust
if any_failed {
    return Vec::new();
}
```

This discards every match, including ones that DID resolve successfully at other levels. The
caller (`call_method_all_with_values`) then can't distinguish "no candidate anywhere matched" from
"some candidates matched, but this one defining level's own candidate set doesn't cover these
args" — both look like an empty candidate list, and `X::Multi::NotFound` is raised even though a
real call site of `.*name`/`.+name` should get back the successful matches.

## How this was found

Discovered 2026-08-15 while implementing (and then reverting) a fix for
`todo/deep/method-entries-never-covers-unpunned-roles.md` — see that file's "Update 2026-08-15"
section for the full trace. That specific change made an un-punned role's own MRO level newly
visible to `get_method_overloads`, which is what made this pre-existing gate actually fire in
practice; the gate itself is independent of that change and file, and worth fixing on its own
merits, not as a side effect of populating `Registry::method_entries` for roles. **Do not use this
ticket to justify populating role-owner rows into the shared method-entries table** — the two
concerns are being kept deliberately decoupled (see the ADR-0019 F4 box's F4a/F4b/F4c split for
why).

## Minimal repro (no Test module, no role-gap fix needed)

Not yet re-confirmed as reproducing on a totally clean `main` (the trace above happened with an
in-progress, since-reverted code change active) — this needs to be re-verified from scratch as the
very first step of picking this up. If it turns out this exact repro needs an un-punned role's MRO
level to be visible (i.e. does NOT reproduce on clean `main` today), the bug may currently be
latent/unreachable via ordinary composed-role dispatch and only surfaces once some future change
(this ticket's own fix, or F4a/b) makes more MRO levels visible to `get_method_overloads` — in that
case this ticket becomes a "must fix before/alongside" prerequisite for that future change rather
than an independently-reachable bug today. Confirm which case this is before designing a fix.

```raku
role R5 {
    multi method rt()       { say 'empty' }
    multi method rt(Str $a) { say 'Str'   }
}
role R6 {
    multi method rt(Numeric $a) { say 'Numeric' }
}
class C { has @.order }
my C $b1 .= new();
$b1 does (R5, R6);
$b1.*rt;
```

`raku` prints `empty`. On the branch with the (reverted) role-method_entries-population change,
mutsu threw `X::Multi::NotFound` ("No matching candidates for method: rt") instead — `R6`'s own
`rt(Numeric $a)` doesn't match zero args, and that single failure discarded the class-level
flattened match (`rt()`) and `R5`'s own matching `rt()`, both of which had resolved correctly.

## Suggested fix direction

Change the multi-candidate branch of `resolve_methods_per_mro_level` so a defining level's failure
to produce a matching candidate for this call's arguments is not fatal to the whole result — only
levels that resolve should contribute to the output, mirroring how a single-dispatch multi-method
call already tolerates "this MRO level's overloads exist but none match, keep walking" (see
`resolve_method_with_owner_impl`'s own loop). The `any_failed` flag's current all-or-nothing
semantics may have been written on the (previously true) assumption that `defining_levels` only
ever contains levels the receiver can actually resolve `method_name` on with *some* signature
compatible with any call — worth understanding why that assumption held before removing it, in
case there's a real invariant (e.g. distinguishing "signature mismatch" from "ambiguous multi
match," which should probably still propagate as an error) that a naive "just skip failures" fix
would silently break.

Raku-verify the desired `.*`/`.+` semantics across a few shapes (a level whose candidates don't
cover the args at all vs. a level with an ambiguous match vs. a level with a private-only
candidate) before landing — this sits on real production dispatch, not introspection, so the usual
"measure before assuming" discipline applies.
