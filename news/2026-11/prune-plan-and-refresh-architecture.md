# `PLAN.md` pruned to open work, `docs/architecture.md` refreshed

`PLAN.md` is meant to hold future tasks only, but it had collected finished
items and progress figures. Removed: RakuAST Stage 0 (#10733), the
`Interpreter` split's finished phases 1 and 2, the regex performance bullet
(its Stage 0 issues are closed, the prefilter landed, and ADR-0135's compiled
program replaced the tree walk), the regex-walk metric, the Digest RIPEMD
note, and the "split the roast fudge logic" non-item. The JSON::Fast item now
points at the open trackers (#8673, #14774) instead of the closed ADR-0110
issue, and dated counts (ecosystem percentages, field and op tallies, the
slow-path survey) give way to the issue or ADR that keeps the live number.
The roast section states roast's current role, a regression guard, with the
1440 / 1455 whitelist figure.

`docs/architecture.md` had stale numbers and one stale claim: `Value` is a
NaN-boxed handle read through `ValueView`, not a 25-variant enum; the AST,
opcode and per-directory file counts were re-measured; `RuntimeError` lives in
`value/error.rs`; and the slang section no longer says the parser cannot
switch slangs, pointing at ADR-0026 and ADR-15762 instead.
`docs/todo-issue-map.md` stays: it is the frozen resolver for the `todo/...md`
paths still cited by older ADRs, news entries and source comments.
