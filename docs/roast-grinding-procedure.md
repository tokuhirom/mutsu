# Roast Grinding Procedure (Subagent Approach)

Goal: Pass all 1296 roast test files using parallel subagents with worktree isolation.

Current status (2026-03-04): 574/1296 passing (44.3%), 722 remaining.

---

## Overview

| Category    | Count | Strategy |
|-------------|-------|----------|
| Pass        |   574 | Done     |
| Fail        |   560 | Parallel by synopsis (Phase 2) |
| Parse Error |   134 | Sequential, parser-focused (Phase 1) |
| Timeout     |    18 | Mixed concurrency + parser (Phase 3) |
| Error       |    10 | Triage individually (Phase 3) |

---

## Phase 1: Parse Errors (134 files) — Sequential

Parse errors touch `src/parser/` which is shared across all tests. Running multiple parser-changing agents in parallel causes merge conflicts. Handle these **sequentially**.

### Distribution

| Synopsis | Count | Key issues |
|----------|-------|------------|
| S02-types | 18 | Type syntax edge cases |
| S03-operators | 14 | Operator parsing |
| S02-literals | 7 | Literal syntax |
| S09-typed-arrays | 5 | Typed array decl syntax |
| S03-metaops | 5 | Meta-operator parsing |
| S32-io | 6 | IO-related syntax |
| S32-list | 5 | List syntax |
| S32-num | 4 | Numeric syntax |
| Others | 70 | Scattered across synopses |

### Procedure

1. Run `./scripts/pick-next-roast.sh -n 1` (it prioritizes parse errors).
2. Dispatch **one** subagent (worktree isolation) to fix the parse error.
3. Wait for PR to merge before dispatching the next one.
4. After every 10-15 fixes, re-run `./scripts/roast-history.sh` to check for cascade improvements (one parser fix often resolves multiple test files).
5. Repeat until parse error count drops below ~20, then move to Phase 2.

### Batching optimization

Group parse errors by root cause before fixing:
```
# Sample parse errors for a synopsis
cat tmp/roast-parse-error.txt | grep S02-types
```
Run the first file with `timeout 30 target/debug/mutsu --dump-ast <file>` to see the parse error message. If multiple files fail on the same syntax construct, fix once and many resolve.

---

## Phase 2: Fails (560 files) — Parallel by Synopsis

The main bulk. These tests parse successfully but have failing subtests. Group by synopsis for conflict-free parallel execution.

### Synopsis Groups (conflict-free parallel slots)

Each group touches primarily different source files and can run as an independent subagent.

#### Group A: String/Text (76 files)
- S32-str (26 fail + 3 parse + 4 timeout + 7 error = 40)
- S15-nfg (12 fail)
- S15-unicode-info (3 fail)
- S15-string-types (1 fail)
- **Primary files**: `builtins/methods_0arg/str.rs`, `builtins/unicode.rs`, `runtime/builtins_string.rs`

#### Group B: List/Array (46 files)
- S32-list (26 fail + 5 parse)
- S32-array (10 fail + 2 parse)
- S09-typed-arrays (5 fail + 5 parse)
- S09-multidim (5 fail)
- S09-autovivification (2 fail)
- S09-subscript (1 fail + 1 parse)
- S09-hashes (1 fail)
- **Primary files**: `builtins/methods_0arg/array.rs`, `builtins/methods_0arg/list.rs`, `runtime/builtins_array.rs`

#### Group C: IO (54 files)
- S32-io (33 fail + 6 parse)
- S16-io (15 fail)
- **Primary files**: `runtime/builtins_io.rs`, `runtime/builtins_system.rs`

#### Group D: Numeric/Math (13 files)
- S32-num (9 fail + 4 parse)
- **Primary files**: `builtins/arith.rs`, `builtins/methods_0arg/num.rs`

#### Group E: Hash (8 files)
- S32-hash (5 fail + 3 parse)
- **Primary files**: `builtins/methods_0arg/hash.rs`, `runtime/builtins_hash.rs`

#### Group F: Temporal (9 files)
- S32-temporal (9 fail)
- **Primary files**: `builtins/methods_0arg/temporal.rs`, `runtime/builtins_temporal.rs`

#### Group G: Types/Literals (34 files)
- S02-types (24 fail + 18 parse)
- S02-literals (10 fail + 7 parse)
- S02-names (3 fail + 4 parse)
- S02-magicals (7 fail)
- **Primary files**: `runtime/types.rs`, `runtime/utils.rs`, `value.rs`

#### Group H: Operators (19 files)
- S03-operators (19 fail + 14 parse)
- S03-smartmatch (5 fail)
- S03-binding (5 fail)
- S03-metaops (1 fail + 5 parse)
- S03-junctions (2 fail + 2 parse)
- S03-sequence (2 fail)
- S03-buf (2 fail)
- **Primary files**: `builtins/arith.rs`, `runtime/builtins_operators.rs`, `compiler/expr.rs`
- **Conflict with Group D**: Both touch `builtins/arith.rs`. Run sequentially or coordinate.

#### Group I: Control Flow (15 files)
- S04-phasers (15 fail + 1 parse)
- S04-statements (12 fail + 2 parse)
- S04-declarations (6 fail + 3 parse)
- S04-blocks-and-statements (4 fail)
- S04-exceptions (3 fail + 1 parse)
- S04-exception-handlers (2 fail)
- S04-statement-modifiers (0 fail + 2 parse)
- **Primary files**: `runtime/control.rs`, `compiler/stmt.rs`, `vm/vm_control_ops.rs`

#### Group J: Regex/Grammar (29 files)
- S05-modifier (12 fail + 2 parse)
- S05-metasyntax (7 fail)
- S05-capture (5 fail + 1 parse)
- S05-match (4 fail)
- S05-grammar (4 fail)
- S05-mass (3 fail + 1 parse)
- S05-transliteration (2 fail + 1 parse)
- S05-metachars (2 fail)
- S05-nonstrings (1 fail)
- **Primary files**: `runtime/regex.rs`, `runtime/regex_parse.rs`, `runtime/grammar.rs`

#### Group K: Signatures/Subs (14 files)
- S06-signature (14 fail + 3 parse)
- S06-advanced (6 fail + 2 parse)
- S06-multi (5 fail + 3 parse)
- S06-traits (3 fail)
- S06-other (3 fail + 2 parse)
- S06-operator-overloading (1 fail + 3 parse)
- S06-routine-modifiers (2 fail)
- S06-parameters (1 fail)
- S06-currying (0 fail + 1 parse)
- **Primary files**: `runtime/dispatch.rs`, `runtime/calls.rs`, `compiler/helpers.rs`

#### Group L: Classes/OOP (39 files)
- S12-methods (13 fail + 1 parse)
- S12-attributes (11 fail + 1 parse)
- S12-class (10 fail + 2 parse)
- S12-introspection (6 fail)
- S12-construction (6 fail)
- S12-enums (3 fail + 2 parse)
- S12-meta (3 fail)
- S12-traits (2 fail)
- S12-subset (1 fail + 2 parse)
- S12-coercion (1 fail + 1 parse)
- **Primary files**: `runtime/class.rs`, `runtime/registration.rs`, `runtime/methods.rs`
- **Conflict with Group M**: Both touch `runtime/registration.rs`. Run sequentially or coordinate.

#### Group M: Roles (13 files)
- S14-roles (13 fail + 2 parse)
- S14-traits (2 fail + 1 parse)
- **Primary files**: `runtime/registration.rs`, `runtime/role.rs`

#### Group N: Concurrency (33 files)
- S17-supply (33 fail + 1 parse)
- S17-promise (7 fail)
- S17-lowlevel (7 fail)
- S17-scheduler (4 fail)
- S17-procasync (2 fail + 1 parse)
- S17-channel (1 fail)
- **Primary files**: `runtime/concurrency.rs`, `runtime/builtins_async.rs`

#### Group O: Modules (6 files)
- S11-modules (6 fail + 1 parse)
- S11-repository (2 fail)
- S11-compunit (1 fail)
- S10-packages (5 fail + 1 parse)
- **Primary files**: `runtime/modules.rs`, `precomp.rs`

#### Group P: Documentation (16 files)
- S26-documentation (16 fail + 2 parse)
- **Primary files**: `parser/pod.rs` (if exists), `runtime/pod.rs`

#### Group Q: Other (misc)
- S32-basics (3 fail), S32-container (2 fail + 1 parse), S32-encoding (2 fail), S32-scalar (1 fail + 1 parse), S32-exceptions (1 fail + 1 parse)
- S24-testing (5 fail), S19-command-line (2 fail), S29-* (scattered)
- **Primary files**: Various, low conflict risk

### Parallel execution plan

Safe to run simultaneously (no file conflicts):

| Slot | Groups | Notes |
|------|--------|-------|
| 1 | A (String) | Independent |
| 2 | B (List/Array) | Independent |
| 3 | C (IO) | Independent |
| 4 | F (Temporal) | Independent |
| 5 | I (Control) | Independent |
| 6 | J (Regex) | Independent |
| 7 | N (Concurrency) | Independent |

Run sequentially or in later rounds:
- D (Numeric) + H (Operators) — share `arith.rs`
- L (Classes) + M (Roles) — share `registration.rs`
- G (Types) — touches `value.rs` which many groups may also touch
- K (Signatures) — touches `dispatch.rs` which Classes may also use

### Procedure per subagent

1. Dispatch subagent with `isolation: "worktree"` and a prompt like:

```
Work on failing roast tests in [Group X]. Files:
[list of roast test file paths]

For each file:
1. Run with raku first: raku <file>
2. Run with mutsu: timeout 30 target/debug/mutsu <file>
3. Compare outputs, identify the gap, fix mutsu.
4. Only modify files in: [list of allowed source files]
5. Do NOT modify parser files unless absolutely necessary.
6. Run make test && make roast after changes.
7. Create a feature branch, commit, push, open PR.
8. Watch CI with: gh pr checks <pr> --watch
9. If CI fails, fix and push again.

Work on 3-5 test files per PR to keep PRs reviewable.
```

2. Monitor PRs: `gh pr list --state open`
3. After each round of PRs merges, re-run `./scripts/roast-history.sh` to find cascade improvements.

---

## Phase 3: Timeouts & Errors (28 files)

### Timeouts (18 files)

| Synopsis | Count | Likely cause |
|----------|-------|--------------|
| S32-str | 4 | Infinite loop in string methods |
| S17-* | 6 | Blocking concurrency |
| S16-filehandles | 2 | Blocking IO |
| S05-* | 2 | Regex backtracking |
| S03-* | 2 | Operator infinite loop |
| S12-methods | 1 | Method dispatch loop |

Handle timeouts individually. Run with `MUTSU_TRACE=1` to identify where the hang occurs.

### Errors (10 files)

These produce no valid TAP plan. Usually a crash or early exit. Investigate one by one.

---

## Conflict Avoidance Rules

1. **Parser files** (`src/parser/**`): Only ONE agent at a time may modify parser files. All others must leave parser files untouched.
2. **value.rs**: Shared by many groups. Changes to Value enum must be coordinated. Prefer adding methods rather than changing the enum.
3. **registration.rs**: Shared by S12 (Classes) and S14 (Roles). Run these groups sequentially.
4. **arith.rs**: Shared by S03 (Operators) and S32-num (Numeric). Run sequentially.
5. **methods.rs** / **methods_mut.rs**: Broad dispatch files. Multiple groups may touch them. Keep changes minimal and localized.
6. **ast.rs** / **opcode.rs** / **compiler/**: Adding new AST nodes or opcodes affects all groups. Only one agent at a time should change these.

### Safe modification zones per group

Each group should ONLY modify files in its designated zone. If a fix requires changes outside the zone, the subagent should document it and skip to the next test file.

---

## Monitoring & Progress Tracking

1. After each batch of PRs merges:
   ```
   git pull origin main
   ./scripts/roast-history.sh
   diff <(sort roast-whitelist.txt) <(sort tmp/roast-pass.txt) | grep '^>' | sed 's/^> //'
   ```
   This shows newly passing tests to add to the whitelist.

2. Track progress in `TODO_roast/` files — update pass/fail counts per synopsis.

3. Expected progression:
   - Phase 1 completion: ~650-700 passing (parser fixes cascade)
   - Phase 2 mid-point: ~900 passing
   - Phase 2 completion: ~1100-1200 passing
   - Phase 3 + cleanup: ~1250+ passing
   - Long tail (concurrency, edge cases): final push to 1296

---

## Subagent Dispatch Template

```
Agent(
  subagent_type: "general-purpose",
  isolation: "worktree",
  prompt: """
  You are working on the mutsu Raku interpreter. Fix failing roast tests
  in [SYNOPSIS GROUP].

  Test files to work on (pick 3-5 per PR):
  [FILE LIST]

  Allowed source files to modify:
  [FILE LIST]

  DO NOT modify:
  - src/parser/** (unless you are the parser agent)
  - src/ast.rs, src/opcode.rs (shared infrastructure)
  - Files outside your allowed list

  Steps:
  1. cargo build
  2. For each test file:
     a. raku <file> — see expected output
     b. timeout 30 target/debug/mutsu <file> — see actual output
     c. Identify the gap and fix
  3. make test && make roast — no regressions
  4. Create branch, commit, push, open PR
  5. gh pr checks <pr-number> --watch — wait for CI
  6. If CI fails, fix and push
  """
)
```

---

## Key Principles

1. **One parser agent at a time.** Parser changes are the highest-conflict risk.
2. **3-5 test files per PR.** Keeps PRs reviewable and reduces conflict scope.
3. **Re-run roast-history after every merge batch.** One fix often cascades to multiple tests.
4. **Never skip a test because it's hard.** Every test must eventually pass.
5. **Check with raku first.** Always verify expected behavior before implementing.
6. **No stubs or hacks.** Every fix must be a genuine, general-purpose improvement.
