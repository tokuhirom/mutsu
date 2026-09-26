# AGENTS.md

Guidance for coding agents (Claude Code, Codex, ...) working in this repository. It is the
**single** instruction file: there is deliberately no `CLAUDE.md` — Claude Code reads `AGENTS.md`
only when a project has no `CLAUDE.md` (since v2.1.277), so adding one back would hide this file
from it. Tool names below (`run_in_background`, `Agent`, `subscribe_pr_activity`, the GitHub MCP
tools) are Claude Code's; another harness uses its own equivalent.

This file holds the rules that apply to **every** session. Procedures used only by some tasks live
in skills, and reference material lives in `docs/`; both are indexed below — read the matching
one before starting such a task.

mutsu is a Rust implementation of a Raku (Perl 6) compatible interpreter:
Source → **Parser** (`src/parser/`) → **Compiler** (`src/compiler/`) → **bytecode VM** (`src/vm/`).

## Start here

Read this file in full before planning or changing code, then read the task-relevant primary
material: the selected issue, its linked ADRs and design documents, and the affected code/tests.
Re-check ADR status lines rather than relying on an old issue's description of them.

### Skills (`.agents/skills/`)

| Skill | Read it when |
| --- | --- |
| [`mutsu-ticket-flow`](.agents/skills/mutsu-ticket-flow/SKILL.md) | Working `todo:ticket` issues end-to-end through merge |
| [`issue-backlog-pipeline`](.agents/skills/issue-backlog-pipeline/SKILL.md) | Orchestrating several issues at once with parallel worktree sub-agents |
| [`roast-triage`](.agents/skills/roast-triage/SKILL.md) | Choosing the next roast target, or investigating one failing `roast/*.t` |
| [`debugging`](.agents/skills/debugging/SKILL.md) | Before adding any `eprintln!`: AST dumps, `MUTSU_TRACE`, scripted `rust-gdb` |
| [`perf-tuning`](.agents/skills/perf-tuning/SKILL.md) | Profiling, A/B-measuring and landing a perf change — a `todo:perf` issue, or before quoting any performance number |
| [`rakuast-implementation`](.agents/skills/rakuast-implementation/SKILL.md) | A RakuAST compatibility slice (`src/rakuast/`, `t/rakuast/`) |
| [`ecosystem-dist-fix`](.agents/skills/ecosystem-dist-fix/SKILL.md) | Making one zef distribution's own test suite pass, or working a red/`blocked_load` `ecosystem/` record |
| [`ecosystem-dist-roulette`](.agents/skills/ecosystem-dist-roulette/SKILL.md) | Picking a *random* distribution and locking it on the board so parallel agents do not collide |
| [`clippy-clone-sweep`](.agents/skills/clippy-clone-sweep/SKILL.md) | A "clone sweep" / `clippy::nursery` pass for wasted `.clone()` calls |
| [`cut-release`](.agents/skills/cut-release/SKILL.md) | Releasing: picking the version, firing `tag-release.yml`, verifying tarballs/npm/Release |
| [`install-raku`](.agents/skills/install-raku/SKILL.md) | `raku` is missing and the Rakudo oracle needs installing |
| [`reclaim-disk`](.agents/skills/reclaim-disk/SKILL.md) | Disk is filling up: stale agent worktrees, `target/` caches |

### Reference docs

| Doc | What it holds |
| --- | --- |
| [docs/architecture.md](docs/architecture.md) | Module map: data types, method dispatch tiers, compiler/VM layout, slangs, GC/JIT status |
| [docs/parser-overview.md](docs/parser-overview.md) | Parser dispatch order, precedence, extension checklist |
| [docs/raku-doc-guide.md](docs/raku-doc-guide.md) | Which `raku-doc/` file answers which language/type question |
| [docs/agent-environments.md](docs/agent-environments.md) | Local box vs. remote container: `gh` → MCP tool map, provisioning, cores, container-only roast failures |
| [docs/ci-pipeline.md](docs/ci-pipeline.md) | CI job layout, the docs-only skip and its allowlist, cancelled-run aggregators |
| [docs/issue-workflow.md](docs/issue-workflow.md) | The GitHub-issue work queue: labels, tiers, the full claim protocol |
| [docs/flaky-test-policy.md](docs/flaky-test-policy.md) | Quarantine policy, flake history, the suspected-flake triage protocol |
| [docs/t-directory-layout.md](docs/t-directory-layout.md) | Which `t/` category a new test goes in |
| [docs/complexity-annotations.md](docs/complexity-annotations.md) | The `// Cost:` comment format |
| [docs/adr/](docs/adr/) | Architecture decisions (`README.md` has the conventions) |

## Hard rules

These are absolute; if a task seems to require breaking one, stop and ask the user.

- **Do NOT add new slow-path / tree-walk fallbacks.** Implement a feature in the compiler (emit
  bytecode) and the VM (execute it). Do not route new features through `call_method_with_values()`
  → `run_instance_method()`, `eval_block`, or the other `runtime/methods.rs` slow-path handlers;
  existing fallbacks are debt, not precedent. If a temporary slow path is truly unavoidable, mark it
  `// TODO: compile to bytecode`.
- **Providing an ecosystem module "natively" (BATTERIES.md rung 3) is BANNED** (user decision,
  2026-08-01; [ADR-0096](docs/adr/0096-batteries-adoption-policy.md)). When a module needs deep
  machinery (Metamodel, EXPORTHOW, slangs, NQP guts), grow the interpreter until the real, vendored
  upstream module runs verbatim (rung 2). Neither "the implementation is large" nor "the real
  module is slow" justifies a native substitute — a measured gap justifies a transparent,
  semantics-preserving optimization only (ADR-0096 §D3). The exceptions are enumerated in ADR-0096
  §D4: `NativeCall` (justified; measured not retirable, [#7560](https://github.com/tokuhirom/mutsu/issues/7560))
  and the JSON `to-json`/`from-json` interception (scheduled for retirement,
  [#8183](https://github.com/tokuhirom/mutsu/issues/8183)). Retire a provider the way `Pod::To::Text`
  (`docs/batteries/pod-to-text.md`) and the native `monitor` declarator
  (`news/2026-08/exporthow-declare-mop.md`) were retired — but measure before assuming it is retirable.
- **No stubs, hardcoded outputs, early returns or test-specific hacks** to make a test pass. Every
  fix is a genuine, general-purpose improvement.
- **Never modify vendored trees**: `roast/`, `raku-doc/`, `old-design-docs/` (update only via
  `scripts/update-vendor.sh`, see `docs/vendoring.md`) and `vendor/zef/` (re-vendor per
  `vendor/README.md`). Zef is the compat north star: fix mutsu, not zef.
- **Only ever touch `tokuhirom/mutsu`.** Never open a PR or file, label, comment on or close an
  issue in any other repository — above all not a Raku-org one (`roast`, `raku-doc`, `rakudo`),
  where an AI has actually mis-filed a mutsu issue before.
- **Never commit to `main`**; always a feature branch and a PR. Never stack PRs (`gh stack`).
- **Preserve unrelated working-tree changes**, and never use a destructive Git operation
  (`reset --hard`, `checkout -- .`, `clean -f`) to discard work you did not create.
- **Never close a PR without preserving its knowledge.** A conflicted PR gets rebased (by you or an
  agent that reads its diff), not closed and summarized.
- **Never remove a test from `roast-whitelist.txt` because of a regression** — fix the regression.
- **Repository artifacts are always English**: code comments, commit messages, PR titles and
  bodies, ADRs, `news/`, `PLAN.md`, `TODO_roast/`, everything under `docs/`. Conversing with the
  user in Japanese does not change this.

## Where this session is running

Sessions run in two environments, and the recipes here are written for the first:

- the maintainer's **local dev box** (an LXC container): `gh` installed and authenticated, 12 cores,
  a warm `target/`;
- an **ephemeral remote container** (Claude app / Claude Code on the web): **no `gh`**, direct
  `api.github.com` calls rejected by the session proxy, ~4 cores, a fixed disk allowance, reclaimed
  when the session ends.

`command -v gh || echo remote` settles which — check, don't assume, and don't conclude a task is
impossible because a named command is missing. [docs/agent-environments.md](docs/agent-environments.md)
maps every `gh` command to its GitHub MCP tool (all with `owner: tokuhirom`, `repo: mutsu`); `git`
and every `cargo`/`make`/`prove` command are identical in both. On the local box never wrap `gh` in
`dotenvx run --` (the `GH_TOKEN` in `.env` is stale and would override the working token).

**Both containers are disposable** — commit and push promptly; anything not on `origin` is not saved.
A remote container is provisioned by `.claude/hooks/session-start.sh` (rustc, `raku`, the native
libraries batteries `dlopen`, `bubblewrap`), so **do not hand-install any of them**; if a build dies
with `E0658`, the hook did not run and `.claude/skills/rustc-too-old/SKILL.md` applies. Its disk is a
fixed allowance: a near-zero `df` "Avail" with low "Used" means the allowance is spent; deletes still
work, so free space with the `reclaim-disk` skill and continue.

## Build, run and test

- Build: `cargo build`. Two binaries ship: `mutsu` (`src/main.rs`) and `mzef` (`src/bin/mzef.rs`, a
  thin re-exec shim over vendored Zef — see `docs/mzef-install-pipeline.md`); touching CLI wiring
  or adding a `[[bin]]` means remembering both.
- Run: `./target/debug/mutsu <file>` or `-e '<code>'`. **Always wrap it in `timeout 30`** — parsing
  or execution can hang. `--dump-ast` prints the AST; `--help` lists the rest.
- Module search order, highest first: `use lib` → `-I <path>` → `MUTSULIB` (colon-separated) →
  installed (`mzef` site repo) → bundled batteries.
- Scratch scripts go in `./tmp/` (gitignored), written with the Write tool — never `/tmp/`.
- One TAP test: `prove -e 'target/debug/mutsu' t/<category>/<file>.t`.
- One roast test: `MUTSU_FUDGE=1 prove -e 'target/debug/mutsu' roast/<path>.t`. **`MUTSU_FUDGE=1`
  is required for roast** (fudge directives are only preprocessed with it) and **must never be set
  for ordinary scripts** (a stray `#?rakudo skip` comment would drop a statement).
- Full suites: `make test` (cargo tests + TAP on the release binary) and `make roast` (whitelisted
  roast). **The exit status is the verdict** (both run under `bash -o pipefail`, guarded by
  `check-pipefail`); the logs `tmp/make-test.log` / `tmp/make-roast.log` are where you find *which*
  file failed. Never re-run a suite just to see its output, and never run the same suite twice
  concurrently (they share build locks, logs and harness state).
- `make lint` runs the four configurations CI's `lint-configs` gates on: default clippy, clippy with
  `jit` off, clippy for wasm32, and rustdoc with `-D warnings`. The lefthook pre-commit hook runs
  `cargo fmt` and the *default* clippy only, so **a green hook is not a green CI**: rustdoc alone
  checks intra-doc links (resolved against the enclosing *module* — inside `impl Compiler` write
  ``[`Compiler::method`]``), and a type whose shape differs per feature can make any file lint
  differently in a configuration you did not compile. Needs `rustup target add wasm32-unknown-unknown`.
- `raku` is the reference oracle: `raku -e '<code>'` whenever expected behavior is unclear, and run
  a roast file under `raku` before comparing mutsu's output. `docs/raku-doc-guide.md` indexes the
  vendored docs; `old-design-docs/` holds the original synopses.

### Before publishing a PR

Run `cargo fmt --all`, `make lint`, `make test` and `make roast` once each, and **do not publish
until all exit zero.** CI is the net for what you could not foresee, not the way to find out whether
your change works. While iterating, run only the tests your change touches. The exceptions:

- A **documentation-only** change (CI skips the build jobs too, see `docs/ci-pipeline.md`): verify
  with `git diff --check`, plus a focused check only if it touches generated output, an executable
  script or test configuration. Re-triaging an issue touches no files and needs nothing.
- A red `make roast` whose failing files are a subset, **by name**, of the container-only list in
  `docs/agent-environments.md` (`uid 0` chmod tests, one sandboxed-network test). Anything else
  failing is yours — never dismiss a whitelisted failure as "pre-existing".

A local timeout on a heavy file under a *debug* build (~3.3x slower than release) is not by itself a
failure; confirm on `target/release/mutsu`. Wall-clock figures in the repo (`make lint` ≈ 5 min)
are 12-core numbers — budget more on a smaller box; that makes the gate slower, not optional.

### Waiting for long jobs — the 30-minute polling floor

Start `cargo build --release`, `make test`, `make roast` and the like with `run_in_background: true`
and **wait for the completion notification.** Polling a running job more often than every 30
minutes is forbidden — tailing the log, `grep -c ' ok$'`, `pgrep rustc`, `sleep`-then-check loops.
One session burned tens of thousands of tokens re-checking an empty output file. A progress count
changes nothing; only a finished run does. A check before 30 minutes needs a real question (a
suspected hang) and is one command. While waiting, do genuinely independent work or end the turn
silently — no per-check "waiting…" messages.

## Code rules

- **Every built-in method / routine / `nqp::` op / VM opcode states its complexity** in a
  `// Cost: O(..), <var> = <meaning>.` line directly above it (user decision, 2026-09-23; format in
  [docs/complexity-annotations.md](docs/complexity-annotations.md)). For an opcode — its `OpCode::`
  arm in `exec_one_dispatch` (`src/vm/vm_exec_dispatch.rs`) plus the `exec_*_op` handler — state
  the per-execution cost; it must not scale with anything unrelated to its operands (frame locals,
  env size, registry size, MRO or call depth). If a bound is worse than Rakudo's or MoarVM's, add a
  `Rakudo: O(..)` / `MoarVM: O(..)` suffix **and file a `todo:perf` issue** (`-- see #NNNN` with the
  real number). A fix that removes a deficit drops the suffix and re-runs the family's
  `scripts/*-complexity-check.sh` case. `nqp::`, `Str`, `Array`/`List` and every opcode arm are
  fully annotated; other families are annotated as they are touched, and a whole-family audit
  follows `news/2026-09/*-complexity-audit.md`.
- **A primitive has exactly one implementation**, shared by every layer that exposes it — method,
  routine/operator form, `nqp::` op, VM opcode, reduction/metaop fold, TRIR op (user decision
  2026-09-23; [ADR-0117](docs/adr/0117-str-methods-and-nqp-ops-share-one-routine.md),
  [ADR-0118](docs/adr/0118-int-operators-share-one-routine.md)). Per-layer copies drifted before
  (codepoint vs. grapheme indexing, Euclidean vs. floored `div`, a missing `$min div -1` guard). The
  homes are [`src/builtins/str_prim/`](src/builtins/str_prim/mod.rs) and
  [`src/builtins/arith/`](src/builtins/arith.rs); callers differ only in how they report an edge
  case (nqp's `_i` ops wrap by contract and stay in `runtime::nqp_pure`). `make check-prims` fails on
  a hand-rolled copy; `t/vm/nqp-str-prim-parity.t` and
  `t/types/numeric/int-operator-forms-parity.t` pin that every form agrees.
- **A name derived from other names is built once, not per execution.** Use
  [`src/qualified.rs`](src/qualified.rs) (`qualified`, `package_ancestors`, `is_qualified`,
  `is_global_package`) instead of run-time `format!("{pkg}::{name}")` / `contains("::")`, and
  `MetaNs` (`src/runtime/meta_ns.rs`) for `__mutsu_*` keys. `make check-name-scans` is a shrinking
  ratchet and `make check-magic-keys` a ban; `src/parser/` and `src/compiler/` are exempt.
- **Keep `size_of::<OpCode>()` ≤ 48 bytes** (the `opcode_size_guard` test) — box fat payloads.
- **Core routine or module function?** A function belongs in core only if `raku -e '<call>'`
  resolves it with no `use` (a `use v6.X` pragma still counts as core) **and** it is documented
  under `raku-doc/doc/Type/` (incl. `independent-routines.rakudoc`). `Test::Util` helpers (`is_run`,
  `doesn't-hang`, `make-temp-dir`, ...) come from `roast/packages/Test-Helpers/`, not core.
  `Language/perl-func.rakudoc` is a Perl 5 migration table, not a builtin index.
- Raku regex is not Perl 5 regex; never assume compatibility.
- Rust files: aim under 500 lines; don't grow one past it (split in the same PR). rustfmt defaults,
  standard naming (`snake_case`, `CamelCase` types, `SCREAMING_SNAKE_CASE` constants). Prefer ASCII in source. Don't rewrite or reformat unrelated code.
- **Every feature or fix carries a test**: a focused `.t` in the right `t/` category
  ([docs/t-directory-layout.md](docs/t-directory-layout.md), enforced by `make check-t-layout`; never
  at `t/` top level), or `#[test]` for internal helpers. Never hardcode a port in a test — listen on
  0 and read `.socket-port`.
- A temporary workaround carries a `// TODO:` saying what the correct approach is and why this one
  falls short.
- Create and edit files with the Write/Edit tools, not shell redirection or heredocs; read with
  the Read tool and search with the harness's search tool (or `git grep` / `rg`).
- **Don't printf-debug** (`eprintln!` → rebuild → repeat): a breakpoint tests a hypothesis without
  a rebuild. Use the `debugging` skill; remove any debug print before committing.

## Design judgment

### Gain and risk

Use these definitions when weighing a change — not a vague "effort vs payoff", and not micro-perf:

- **Gain** = moving toward the *correct architecture*: a maintainable, fast interpreter with **no
  flaky tests** and **better Raku compatibility**. Removing a band-aid, unifying a dual mechanism or
  making a subsystem sound is a gain even with a modest speedup.
- **Risk** = making the codebase *worse*: **flaky tests**, **reduced compatibility**, **lower
  maintainability**, **ad-hoc / band-aid changes**.
- **A temporary CI/roast failure is NOT a risk.** Roast catches it deterministically and you fix
  forward.

So prefer **sound mechanisms that cannot go flaky** over optimizations correct only under an
*incomplete* static analysis. Example: capturing a mutable lexical as a shared cell always tracks
later writes; snapshotting it by value is right only if no write is ever missed — and mutsu's
mutation analysis misses writes from separately-registered methods and rw-arg sinks like `cas`
(the `S12-construction/roles-6e.t` regression). The cell is the gain; the snapshot is the risk. A
known-hard prerequisite is itself a gain to pursue, not a reason to stop.

### Refactor boldly

CI and the roast suite gate every merge. **Do not hide behind "too risky — I'll ship a tiny slice /
just document the design."** When the right fix is a substantial refactor, do the real change in
full, iterate on targeted tests, pass the pre-publication gate, and fix forward on the branch if CI
still catches something. One coherent architectural PR beats ten micro-PRs dancing around the
problem. Do not fear complex features: when a test needs several unrelated features, implement
them all in the same PR.

### ADRs

Costly-to-reverse decisions are recorded as ADRs in `docs/adr/` (`NNNN-title.md`). Read the
relevant ADR before touching its area. Before making a new large architectural call, write a
`Proposed` ADR instead of baking it silently into code; supersede, never rewrite, an ADR whose
decision changes. GC (cycle collector), NaN-boxing and the JIT are **shipped and default on**
(ADR-0001 §7) — not pending work; a MoarVM-style moving GC stays rejected without a new ADR. See
`docs/architecture.md` for the current status.

### Trust `main`

`main` only accepts PRs that passed CI. **Do not check whether a failure also happens on `main`** —
a failure on your branch is your change's.

## Roast

- The goal is to pass all of roast. Task selection is **PLAN.md → `TODO_roast/BLOCKERS.md` →
  individual tests** — never cherry-picking easy tests to game the count; the procedure is the
  `roast-triage` skill.
- `TODO_roast/BLOCKERS.md` is the single ledger of non-whitelisted tests. Record why a deferred or
  partially passing test fails in its row (or its investigation notes); remove the row when the
  test reaches the whitelist.
- Whitelist a file only when `prove` exits cleanly on it. Keep `roast-whitelist.txt` sorted
  (`LC_ALL=C sort -c`); CI checks it.
- Roast is the authoritative spec: if passing it means changing a local `t/` test, change the
  local test.
- A whitelisted test failing in `make roast` is investigated, never dismissed as pre-existing.

### Flaky-looking failures

`flaky-tests.txt` (with review dates, enforced by `make check-flaky-list`) is the only quarantine;
listed files are retried automatically and only a pass on retry counts. "Flaky" is a claim of
non-determinism — verify it first: most historical flaky labels were deterministic bugs. Re-run the
file ~5× on a **release** build; a concrete `not ok` is a logic bug; an `exit 255` + `Failed: 0`
that reproduces on your branch and not on `main` is an exception your change throws mid-file. Full
protocol and the flake history: [docs/flaky-test-policy.md](docs/flaky-test-policy.md).

## Git, PRs and CI

1. Branch from `main`; commit; push with `git push -u origin <branch>`.
2. Open the PR (`gh pr create` / `create_pull_request`) with a `type:` or `type(scope):` title —
   it drives the category label and release-note section. No version-bump label.
3. Enable auto-merge with **merge**, not squash (`gh pr merge --auto --merge <n>` /
   `enable_pr_auto_merge` with `mergeMethod: "MERGE"`); squash is disabled on this repository and
   fails silently, leaving auto-merge off. Rebase is also allowed.
4. **Immediately verify it is mergeable.** `DIRTY`, or no CI run registering within ~1 min, means a
   conflict with `main` (usually a sibling PR touching the same doc/ledger file): `git fetch origin
   main && git rebase origin/main`, resolve, `git push --force-with-lease`. A `DIRTY` PR never runs
   CI and never merges.
5. **Watch CI by being woken, never by polling.** Locally: one `run_in_background` command that
   blocks until no check is pending. Remotely: `subscribe_pr_activity` and let CI and reviews wake
   the session. Never foreground `gh pr checks --watch` (it blocks ~13 min; only a harness with no
   background notification at all may block on it once), never `sleep`, never re-read an unfinished
   run inside the 30-minute floor. A red run: fix forward on the same branch and push. Aggregator
   jobs report a cancelled run on a superseded commit as red — judge by the current head
   (`docs/ci-pipeline.md`).
6. **A PR is done when GitHub reports it `MERGED`** and its merge commit is reachable from
   `origin/main` — not when checks pass or auto-merge was requested.
7. **Before going idle, decide the next slice** from `PLAN.md` / `TODO_roast/BLOCKERS.md` / the
   issue queue, or put a strategic fork to the user.

Releases are cut by one manual trigger of `tag-release.yml` — see the `cut-release` skill. All four
release targets (Linux/macOS × x64/arm64) are required; do not weaken one to pass another.

## Issues, planning and news

- **`PLAN.md` holds only future tasks.** Remove an item when it is done.
- **Record each accomplishment as a new file `news/YYYY-MM/<kebab-slug>.md`** (H1 title + prose;
  see `news/2026-07/nil-string-context-warning.md`). One file per entry, never a shared monthly
  file and no index in the month directory — that is what keeps parallel PRs from conflicting.
  Just write the file; the directory is created with it. The monolithic `news/2026-06.md` and
  earlier archives, and `news/2026-07.md`, are frozen.
- **A finding too large for this or the next session becomes a GitHub issue** on `tokuhirom/mutsu`
  (one per finding), not a `PLAN.md` line. Label it `todo:ticket` (small, no design needed),
  `todo:deep` (needs design/an ADR) or `todo:perf` (correct but slow; **must state its close
  condition** — a ratio against rakudo, a return to a prior speed, or the right complexity order —
  announced to the user when work starts, and closed exactly when met). A perf finding that also
  fixes a wrong answer is `ticket`/`deep`. `tier:*` labels come from triage, not the filer. The body
  — root cause, affected files, why it is large, a minimal repro with mutsu's and `raku`'s output —
  must let a cold session pick it up. Details: [docs/issue-workflow.md](docs/issue-workflow.md).
- **Claim an issue before working it** — agents run in parallel. Post a comment whose **first
  line** is `Claiming: <branch>`, re-read the comments, and yield if a live claim with a lower
  comment id exists; the **lowest comment id is the whole tiebreaker** ("I'm further along" or "the
  user pointed me here" do not override it). Re-read twice more: before the full-suite run and right
  before opening the PR. Post `Releasing: <branch>` (same branch name) when merged, stopped or
  blocked. Never set the `working` label yourself — `claim-label.yml` derives it from those comments
  — and never pick up an issue carrying `working` or a live claim.
- Close a resolved issue with `Closes #NNNN` in the PR body, and still write the `news/` entry.

## Agents and queue requests

- **Sub-agents are allowed** where they genuinely fan out: read-only searches (cheap) or
  independent implementation slices (expensive — each worktree agent multiplies build cost and
  disk). At most **3 agents that build** at once and **10 in total** on the 12-core box; one on a
  ~4-core remote container, where inline work usually beats a worktree. Details and rationale: the
  `issue-backlog-pipeline` skill. Clean up agent worktrees at least hourly (`reclaim-disk`).
- **Task selection order:** PLAN.md priorities → BLOCKERS.md highest-impact features → roast tests
  near in-progress work → `todo:deep` and `todo:ticket` issues oldest-first, both queues worked in
  parallel (`tier:S` > `tier:B` > `tier:N`; `tier:icebox` is out). `todo:perf` is batched into its
  own profiling session with a single implementation agent (`perf-tuning` skill).
- **A queue request is a standing instruction.** "Work the `tier:N` tickets and open the PRs",
  "drain the queue" and the like are complete; the mechanics are settled and **never asked back**:
  one issue per PR; selection is the named filter, oldest-first, skipping claimed issues (a missing
  `tier:*` is workable); claim and release every issue; every fix has a regression test, a `news/`
  entry and `Closes #NNNN`; open the PR, enable auto-merge, watch CI, fix forward ("PR を出して" *is*
  the permission); go straight on to the next issue after each verified merge, up to the run cap in
  `mutsu-ticket-flow`; re-triaging to `todo:deep` or closing as already fixed are legitimate
  outcomes. Ask only for a decision genuinely the user's (a rung-3 provider, a new or superseding
  ADR, weakening a CI gate, dropping a whitelisted test, or two materially different readings the
  issue cannot settle) — and even then park that one issue, continue the batch, and raise it in the
  final report.
