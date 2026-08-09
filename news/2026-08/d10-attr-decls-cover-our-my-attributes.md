# ADR-0019 D10 (partial): attr_decls now covers our/my attributes, drops the has-decl raw-Stmt fallback

`class_body_has_decl` and `role_body_has_decl` no longer fall back to
rebuilding a `CompiledAttrDecl` from a raw `Stmt::HasDecl` at registration
time. The compiler's `compile_class_attr_decls` previously excluded a
class-level `our`/`my` attribute from the `attr_decls` table it builds at
plan-lowering time — the reason `class_body_has_decl` needed a raw-statement
fallback in the first place (added in ADR-0019 D6-4, closes D6). Dropping
that exclusion means every `has` declaration, `our`/`my` included, now has a
compile-time descriptor, so the registration-time lookup always hits.

The role side turned out to already have full coverage:
`compile_role_attr_decls` never excluded `our`/`my` attributes to begin
with, so `role_body_has_decl`'s equivalent fallback (added in D9, closes D9)
had actually been dead code since the role attribute-descriptor collector
landed — this PR is what surfaced it via a grep sweep for `from_stmt`
callers.

With both fallbacks gone, `ClassBodyOp::Attr`/`RoleBodyOp::Attr` no longer
need to carry a raw statement at all — they shrink back to a bare
`{ name: Symbol }` marker, the same shape as `Method`/`Does`/`Parent`. Two
stale `#[allow(dead_code)]` annotations left over from `body_plan`'s
"purely additive, no consumer yet" phase (before D6-4/D9 wired it up as the
walk's sole driver) are also removed now that the fields are genuinely
read.

Verified via the full `t/` suite, all Rust unit tests, the
`S12-class`/`S12-construction`/`S14-roles`/`S05-grammar`/`S12-attributes`
roast files (the `S12-attributes/trusts.t` failure is pre-existing per
`TODO_roast/BLOCKERS.md`, unrelated to this change),
`scripts/battery-testsuite.sh`, and a hand comparison against `raku`
covering a class and a role with `our`/`my` attributes plus a nested-sub
`has`.

This closes the two concrete `from_stmt` gaps ADR-0019's D10 box flagged,
but not the box itself — the remaining `Stmt::` reads in the class/role
body walk (`Other`/`ClassSub`/`CodeAlias`/`ProtoMethod`/`LeavePhaser`'s own
`raw` field, `Deferred`'s stub-marker check) are typed-op payload
extraction, not AST-shape dispatch, mirroring the ADR's own accepted C6
`FunctionDef.body` precedent. See the ADR's D10 entry for the open
question of whether that is the intended permanent end state.
