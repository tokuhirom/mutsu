# The class/role registration walkers are split into named phases

ADR-0019 D0 (#5949) — a pure mechanical extraction with zero behavior
change, so D1–D9 can replace one function at a time. `register_class_decl`
was a single ~2,500-line function whose concerns shared ~8 mutable locals,
and `register_role_decl` (~920 lines) had the same shape; the D1–D6 cut
lines run through those function bodies, which is why this slice exists.

Both are now orchestrators over named phase functions with explicit
inputs. The class walker's phases live in `registration_class_validate.rs`
(rollback snapshot `ClassRegSnapshot`, redeclaration/parent validation,
shell publication), `registration_class_compose.rs` and
`_compose_body.rs` (role composition behind a `RoleCompositionCx`,
deferred role bodies, puns), and `registration_class_body*.rs` (the body
walk behind a `ClassBodyCx` context struct: attributes, methods,
`also does`, exit phasers/finalization/EXPORTHOW); the host file is 229
lines. The role walker splits into `registration_role_decl.rs`
(validation, state reset, body pre-scan + dispatch, finish),
`registration_role_body.rs` (`has`/`does` arms), and
`registration_role_method.rs`; host file 334 lines.

Two extraction notes worth keeping. Arm-level `continue`s used to skip the
per-statement registry-republication tail, so an extracted arm returning
normally would have *run* the tail — an ordering change; extracted arms
signal it with an explicit `ClassBodyFlow::{RunTail,SkipTail}` return, and
all 21 `continue`s were audited. Error-path restores are inconsistent by
design (some restore package/env, the duplicate-method error does not;
body-walk errors never trigger the registry rollback) — preserved exactly.
The only consolidation: the byte-identical `our method`/`my method`
invocant munging became one `method_sub_form_params` helper; everything
else is verbatim motion with comments moved alongside.

Validated with the full local `make test` and `make roast`, targeted
S12/S14 roast files, and error-path spot checks. Executed by a worktree
subagent from a one-page brief; the diff was 15 files, +4,326/−3,667,
every new file ≤ 502 lines.
