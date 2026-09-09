# The icebox now says why: an `icebox:*` reason axis beside `tier:icebox`

`tier:icebox` recorded that an issue was out of the queue but not why, and the
seven issues carrying it turned out to be in four genuinely different states.
Two were waiting on a design call this project can make in one conversation
(#7542's itemization representation "wants an ADR paragraph"; #7547's LSP
`references` side table needs a measurement before ADR-0065 D6 can be amended).
Two were waiting on code, not judgement (#7545's R5 sits behind #7554; #7546's
two structural gaps are retired by ADR-0055 slices 3-5). One was a settled
decision kept so it is not re-derived (#7560, NativeCall's 61-op gap, with
explicit reopen conditions). Two were real, understood, and measured as not
worth a session of their own (#7550 has zero corpus hits and rakudo's own
behaviour there is a `VMNull` artifact; #7540's E2 remnant is a non-gating
cleanup behind a monitoring counter).

Those four states differ in **who unblocks them**, which is exactly what a
priority tier cannot express. Under one label the whole icebox read as "ignore
this" — wrong for three of the four, and most wrong for the decision-blocked
pair, which is the only group a maintainer can clear by deciding.

So the icebox reason became its own axis. `tier:icebox` stays as the ranking
value, so the triage regen's contract and every existing listing query are
unchanged; alongside it every iceboxed issue now carries exactly one of
`icebox:decision`, `icebox:blocked`, `icebox:opportunistic` or `icebox:record`.
The label is chosen by one question — *what would make this actionable?* —
which is also what makes relabelling mechanical as the state moves: a
`decision` issue becomes `blocked` once its ADR is written, and a `blocked`
issue leaves the icebox entirely when its blocker merges.

`icebox:blocked` carries one obligation the others do not: the body must name
its blocker as an issue number or an ADR slice. Half the current pair named it
as a `todo/...md` path instead, which stopped resolving at the 2026-09-08
migration — the same rot that motivated moving the backlog to issues in the
first place.

All seven open issues were classified, and the axis is documented in
[`docs/issue-workflow.md`](../../docs/issue-workflow.md); `docs/triage.md`'s
ranking prose, which described the icebox as a single state, was corrected to
match.
