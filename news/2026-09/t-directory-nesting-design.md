# A nested layout for `t/`, and the infrastructure to allow it

`t/` had grown to **3,938 flat `.t` files**. Past a few hundred, a flat directory stops being an
index: `ls t/` is unreadable, and neither a human nor an agent can answer "does a test for this
already exist?" without a full-text grep over the whole tree. [#7819](https://github.com/tokuhirom/mutsu/issues/7819)
asked for the directory to be refactored.

This change is the **design half** of that work. It defines the layout, updates every rule that
tells a contributor where a test goes, and lands the tooling that makes nesting possible — but it
moves no test files. The 3,938 `git mv`s come in a separate, purely mechanical PR. Splitting it
this way is deliberate: a rename of that size is unreviewable if it also carries policy changes,
and the policy has to be in `main` first or every in-flight PR that adds a `t/` file conflicts
with the move.

## The layout

[`docs/t-directory-layout.md`](../../docs/t-directory-layout.md) is the authority. In short:

- Sixteen top-level subject categories — `lang`, `types`, `collections`, `control`, `routines`,
  `oo`, `regex`, `grammar`, `exceptions`, `io`, `modules`, `concurrency`, `nativecall`,
  `rakuast`, `vm`, `tooling` — as a closed set. A test file always lives in one of them and never
  at `t/` top level.
- At most two levels below `t/`. A category may gain subcategories once it passes ~200 files.
- **Basenames stay globally unique, and a migrated file keeps its basename exactly**:
  `t/regex-backtrack.t` becomes `t/regex/regex-backtrack.t`, not `t/regex/backtrack.t`. The
  redundancy is the point. Several hundred `t/<name>.t` references in `docs/`, `news/` and code
  comments are prose rather than links, so nothing rewrites them mechanically; an unchanged,
  unique basename keeps every one of them resolvable by a single `git grep`, and keeps the
  migration a diff a reviewer can check by name. It also means the tools that index tests by
  basename rather than path — `scripts/test-module-sweep.sh`'s flat work directory above all —
  need no rework.
- Placement is decided by **what the test would catch if it broke, not by the syntax it happens
  to use**. A test that writes `for @a { ... }` to prove a closure captures the loop variable by
  reference is a `vm/` writeback test, not a `control/` loop test.
- `lib/`, `fixtures/` and `packages/` are support directories that hold no tests. The nine
  existing `lib-*` / `*-lib` directories are grandfathered and must not grow.

Because tests reach their fixtures by a path relative to the repository root (`-I t/lib`) and
`prove` runs from the root, nesting a test does not change how it loads anything. No test body
needs editing.

## Infrastructure

`prove` does not descend into subdirectories without `-r`, so every invocation of the suite now
passes it: the `make test` recipe and all three CI TAP steps (`test`, `gc-stress`, `jit-stress`).
This is verifiably a no-op on the current flat tree — `prove --dry` and `prove -r --dry` return
the identical 3,938-file list — and `-r` still matches `*.t` only, so the 171 `t/lib/*.rakumod`
fixtures and `t/fixtures/**/*.rakutest` stay invisible to it.

`scripts/check-t-layout.sh` (new; `make check-t-layout`, a `make test` prerequisite and a CI step)
enforces the machine-checkable half of the design: known categories only, the two-level depth cap,
declared subcategories, no `.t` under a support directory, and globally-unique basenames. The
"no `.t` at `t/` top level" rule is present but held off behind a `MIGRATED` flag, so the guard
lands and starts protecting the other five rules before the move rather than after it.

`scripts/test-module-sweep.sh` discovered tests with `ls t/*.t`; it now uses `find`, which is the
one consumer that would silently have swept nothing once files moved.

## Rules updated

`CLAUDE.md` (test-infrastructure section, the feature-test convention, the skills table),
`AGENTS.md` (repository layout and the regression-test rule), and the `test-util-workout` and
`rakuast-implementation` skills all now name a category instead of bare `t/`, and point at the
layout document rather than restating it.
