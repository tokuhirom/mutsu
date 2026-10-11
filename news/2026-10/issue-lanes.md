# The issue backlog is organised into lanes

The open backlog had grown to about 200 issues, and choosing work for parallel agents meant
checking by hand, every time, which issues would touch the same code. On 2026-10-11 the
backlog was cleaned up and then split into lanes.

**Cleanup.** Every bug-shaped issue filed before 2026-10-08 had its repro re-run on `main`
and compared against raku. Ten were already fixed and are closed (#10419, #11013, #11125,
#11193, #11433, #11464, #11516, #11548, #11797, #12150). Thirteen partly fixed ones got a
status comment saying what is left. Two stale claims were released. #7579 was closed as
superseded by #10951 and #10952. The ecosystem lock board was rotated to #12725. The re-check
also exposed a new bug, filed as #12727: `for $channel.list` holds each item back until the
next `send`.

**Lanes.** Each open `todo:*` issue now carries one `lane:*` label naming the area of code its
fix touches. Lanes are cut so that two of them rarely edit the same files, so one agent per
lane is the rule for parallel work. `docs/lanes.md` holds the lane list and the priority
order between lanes. The current focus is the three architecture campaigns, ranked first:
env (ADR-12529), method-table rows (ADR-11276) and RakuAST as the frontend (ADR-10723). The
same doc holds the rule that orders a lane's own queue: root causes first, then tier, wrong
answers before slow ones, `ecosystem` first, then oldest. Campaign lanes follow their carrier
issue's plan instead, and two rules keep other lanes out of their way: do not fix a bug on a
path a campaign is retiring, and write new code the campaign's way.

`scripts/next-issue` prints each lane's next issue from the labels, so the order is computed
when work is picked, not maintained by hand. The user re-prioritises by re-ordering the
lane table.
