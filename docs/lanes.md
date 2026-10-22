# Issue lanes — what to work next, and what can run in parallel

The open-issue backlog is split into **lanes** by the code each issue touches. Every open
`todo:*` issue carries exactly one `lane:*` label. Lanes are cut so that two lanes rarely edit
the same files, which turns the two questions every parallel run asks into lookups:

- **What can run in parallel?** One agent per lane at a time. Two agents in different lanes do
  not conflict; two agents in the same lane would, so a lane with a live claim is busy.
- **What comes next?** The highest-ranked free lane's next issue. `scripts/next-issue` computes
  it from the labels; nobody maintains an ordered list by hand.

The user decided this layout on 2026-10-11, along with the current focus: the
architecture campaigns rank first.

## Lane order

This table is the only thing that sets priority between lanes. Changing focus means
re-ordering its rows; `scripts/next-issue` reads it from this file. *Kind* says how the
lane's next item is chosen (below). *Slots* is how many agents may work the lane at once.

| Rank | Lane | Kind | Carrier | Slots | What it covers |
| ---: | --- | --- | --- | ---: | --- |
| 1 | `lane:env` | campaign | #12529 | 1 | ADR-12529: names resolve through the static link; the frame `Env`, closure capture and flatten machinery, scoping of `$/` / `$!` / `@_` |
| 2 | `lane:rakuast` | campaign | #7564 | 1 | ADR-10723: RakuAST as the frontend IR (`src/rakuast/`, `t/rakuast/`) |
| 3 | `lane:threads` | queue | | 1 | Cross-thread sharing, atomics, Promise/Channel/Supply, GC refcount races |
| 4 | `lane:roles` | queue | | 1 | Role composition and role-body timing, the MOP, `Metamodel::*` |
| 5 | `lane:multi` | queue | | 1 | Multi dispatch: candidate ranking, `callsame`/`nextsame`, `X::Multi::*` |
| 6 | `lane:modules` | queue | | 1 | `use`/`need`/`import`, `EXPORT`, `CompUnit::*`, packages and stashes |
| 7 | `lane:begin` | queue | | 1 | `BEGIN`/compile-time execution (ADR-0134), hoisting, `EVAL` unit semantics |
| 8 | `lane:regex` | queue | | 1 | Regex/grammar engine correctness and speed |
| 9 | `lane:core` | queue | | 2 | Self-contained core-semantics bugs (values, containers, Seq/List, formatting, IO), and the remainder of the built-in method-table work (#11276; `lane:method-table` was retired on 2026-10-22, the bulk being done) |
| 10 | `lane:parser` | queue | | 1 | Parser gaps; fixes land here even during RakuAST Stage 2 (see below) |
| 11 | `lane:nqp` | campaign | #11488 | 1 | `nqp::` op coverage and representation fidelity |
| 12 | `lane:arch` | campaign | #10779 | 1 | Interpreter decomposition, slow-path ratchets, compiler carriers |
| 13 | `lane:tooling` | queue | | 1 | LSP, profiler, REPL, precompilation store, crash reporting, NativeCall storage |
| — | `lane:perf` | perf | | 1 | Every `todo:perf` issue not absorbed by a campaign. Always exactly one solo agent (`perf-tuning` skill), independent of rank |

## How a lane's next item is chosen

**`queue` lanes** are ordered by rule, computed at pick time from the labels:

1. Skip issues that are `tier:icebox`, carry `working`, or are a sub-issue of an open parent
   (a parent is a root cause; its fix is expected to fix or reshape the child, so the child
   waits).
2. A parent with open sub-issues first: fixing it unblocks the most.
3. `tier:S` > `tier:B` > `tier:N` > no tier.
4. A wrong answer (`todo:ticket` / `todo:deep`) before a slow one (`todo:perf`).
5. `ecosystem` before the rest.
6. Oldest first.

An issue labelled `priority` jumps to the head of its lane. Only the user sets it.

**`campaign` lanes** follow their carrier, not the rule. The carrier issue (and its ADR's
migration section) says which slice is next; the other issues in the lane are work items of
those slices. ADR-12529 §7, for example, maps each same-root-cause issue to the phase that
fixes it. Two rules keep the rest of the backlog out of a campaign's way:

- **Do not fix a bug on a path a campaign is retiring.** An issue whose fix would land in the
  old mechanism goes into the campaign's lane (as a sub-issue of the carrier when the ADR
  maps it to a phase) and waits for, or is worked as, that phase.
- **Write new code the campaign's way.** A ticket that adds a built-in method
  adds a method-table row (ADR-11276), never a new name-matching branch; a new lexical lookup goes
  through the static link, never a new env probe. Then the ticket does not conflict with
  the campaign but advances it.

A campaign's progress is measured by the old mechanism that is left, not by what has been
added, because a registered row next to a surviving old branch is not done. Each carrier
states its remaining-old-path count, and a ratchet in `make checks` keeps it from growing
where one exists (#11510 for the slow-path fallbacks).

**`perf`** is one solo agent, per AGENTS.md: parallel perf agents produce measurements that
drift. Its next item follows the queue rule.

## Parser changes during RakuAST Stage 2

RakuAST Stage 2 (the parser emits RakuAST directly) will take a long time, and freezing the
parser for that long is not realistic (user decision, 2026-10-11). So a parser fix may land
from any lane, before and during Stage 2, and `lane:rakuast` absorbs it: the campaign rebases
over parser changes rather than serializing them. A parser fix keeps the RakuAST round trip
green (the files in `ci/rakuast-frontend-passing.txt`) so the two frontends do not drift
apart, and a fix to a construct Stage 2 has already moved is made in the new path.

## Who sets what

The order is mechanical, but its inputs are judgments. Three places make them:

| Who | When | What |
| --- | --- | --- |
| The agent that files an issue | at filing | Its `lane:*` label (a missing one is reported by `scripts/next-issue`). If the issue is a symptom of an open root-cause issue, it is added as that issue's sub-issue. |
| A periodic triage pass | weekly | Labels the unlaned, links symptoms to a root cause once several share one, re-runs old repros and closes what is already fixed, releases stale claims. Seeing that six issues are one cause needs them side by side, which no filer has. |
| The user | when the focus changes | The row order of the table above, and the `priority` label. |

## `scripts/next-issue`

```sh
scripts/next-issue              # every lane in rank order: busy/free and its next issue
scripts/next-issue --free       # only the lanes nobody is working
scripts/next-issue --lane env   # one lane's whole ordered queue
scripts/next-issue --input issues.json   # read issues from a file instead of `gh`
```

On the local box it reads the open issues through `gh`, including each issue's parent. In a
remote container (no `gh`), save the open issues with the GitHub MCP `list_issues` tool
(`state: OPEN`, `fields: [number, title, labels, created_at]`, every page) to one JSON file
and pass it with `--input`. That listing has no parent information, so the script then cannot
skip sub-issues; check `has_parent` with `issue_read` on the issue it names before claiming.

A lane is busy when one of its issues carries `working`; `Slots` above says how many may.
Claiming is unchanged: the `Claiming:` comment protocol in
[issue-workflow.md](issue-workflow.md), lowest comment id wins.
