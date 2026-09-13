#!/usr/bin/env bash
#
# Decide whether a push/PR touches documentation ONLY, so CI can skip the
# ~25-30 min build+roast jobs for a change that cannot possibly affect them.
#
# Prints `true` or `false` on stdout. Always exits 0: any uncertainty
# (unknown event, missing API data, a path not on the allowlist below) prints
# `false`, i.e. run the full suite. A wrong `false` costs runner minutes; a
# wrong `true` lets an untested code change reach main, so every ambiguous
# case must resolve to `false`.
#
# Why an allowlist and not a `paths-ignore` denylist: `paths-ignore` at the
# workflow level makes GitHub never create the check run at all, which leaves
# the required status checks (`test`, `wasm-e2e`, `gc-stress`) pending forever
# and the PR unmergeable. The supported way to skip a *required* check is to
# let the job exist and skip it with a job-level `if:` — a skipped job counts
# as success for branch protection. This script feeds that `if:`.
#
#   scripts/ci-docs-only.sh              # classify the current CI event
#   scripts/ci-docs-only.sh --self-test  # verify the classifier (runs in CI)
#   scripts/ci-docs-only.sh --check-inputs  # no build input may look like a doc
#   printf 'a\nb\n' | scripts/ci-docs-only.sh --classify   # classify a list
#
# It also answers the inverse question for the Miri gate (ADR-0013 §4 phase 4),
# which is expensive (~25 min) and only meaningful for GC/container code:
#
#   scripts/ci-docs-only.sh --gc-value                     # classify the event
#   printf 'a\nb\n' | scripts/ci-docs-only.sh --classify-gc-value
#
# Same fail-safe discipline, opposite default: an unclassifiable diff prints
# `true` there, because a skipped soundness check is a silently-unchecked merge
# while a needless one only costs runner minutes.

set -u

# A path is documentation iff it matches one of these. Everything else --
# src/, t/, roast/, site/, modules/, vendor/, benchmarks/, crates/, tools/,
# Cargo.*, Makefile, roast-whitelist.txt, flaky-tests.txt, and every shell/mjs
# script under scripts/ -- forces the full suite.
#
# The test each entry has to pass is NOT "does this look like prose". It is
# "can any of the five build jobs read this file". `check_inputs` below turns
# that from a claim into something CI derives and enforces, so read it before
# adding an entry here.
#
# Deliberately NOT `**/*.md`: a README under modules/ or site/ sits next
# to files the build reads, and the blast radius of guessing wrong there is a
# silently-untested merge. Top-level *.md (PLAN, README, CLAUDE, ANALYSIS,
# PERFORMANCE, BATTERIES, AGENTS) is safe and covers the common case. Top-level
# *.tsv / *.svg is the same case in a different extension: HISTORY.tsv and
# HISTORY-pass.svg are the roast-history record and its chart, appended by
# scripts/roast-history.sh and read by nothing that builds or tests.
#
# `.claude/**` and `.agents/**` are agent configuration and agent-facing
# documentation (skills, settings). Nothing in the build reads either -- not
# cargo, not `prove`, not the roast runner -- so a change there cannot move a
# single test result, and adding a skill used to cost a full ~25 min suite for
# one markdown file. They are on the allowlist as whole directories rather than
# just their `skills/**` subtrees because the same argument covers everything
# CI ignores.
# (`.agents/` is where this repo's own skills live -- the table at the top of
# CLAUDE.md points at `.agents/skills/` -- so leaving it off meant every
# SKILL.md edit paid the full suite. It was an oversight, not a distinction.)
#
# `ecosystem/**` is the zef-distribution parity ledger (one JSON record per
# distribution, plus history.tsv/.svg and the index snapshot). It is a
# *measurement of* mutsu, never an input to it: the writer is
# .github/workflows/ecosystem-sweep.yml and the only reader is pages.yml, which
# has its own `paths:` trigger on the same tree. A 250-file re-measurement sweep
# used to pay for two cargo builds and three roast runs to confirm that
# recording what mutsu did does not change what mutsu does.
# NOTE the asymmetry with `site/`, which stays OFF the allowlist even for the
# generated ecosystem projection: `site/e2e.test.mjs` (the wasm-e2e job) loads
# `site/ecosystem.html` and cross-checks it against `site/content/ecosystem.json`,
# so those two really are build inputs.
#
# `.github/**` except ci.yml: issue templates, the release-note config, and the
# other six workflows (pages, bench, docker, release, tag-release, label-pr,
# ecosystem-sweep) are read by GitHub, not by any job here -- and each one is
# exercised by its own run, which the five build jobs tell you nothing about.
# `.github/workflows/ci.yml` is the exception and must stay off the allowlist,
# because it *defines* those five jobs: a change to it is precisely the change
# they exist to demonstrate, and skipping them would merge an edit to the test
# pipeline that has never once been executed. (`is_gc_value_path` below lists
# ci.yml for the same reason, in the same direction.)
#
# `scripts/*.py`: the Python under scripts/ is reporting and campaign tooling --
# ecosystem sweeps, roast/bench/backlog plots, manifest generation, one-off
# surveys. None of it is on the `make test` / `make roast` / ci.yml path, with
# the single exception denied below. The shell and .mjs scripts are a different
# story (CI runs run-t-test.sh, run-roast-test.sh, check-site-snippets.sh, ...),
# so scripts/ as a whole stays off the allowlist.
is_doc_path() {
  case "$1" in
    docs/*|news/*|TODO_roast/*|old-design-docs/*|raku-doc/*) return 0 ;;
    .claude/*|.agents/*) return 0 ;;
    ecosystem/*) return 0 ;;
    # Before `.github/*`: order decides, and this one must lose.
    .github/workflows/ci.yml) return 1 ;;
    .github/*) return 0 ;;
    # `make check-t-layout` (a `make test` prerequisite and a CI step) runs it.
    scripts/migrate-t-layout.py) return 1 ;;
    # `make check-panic-surface` (likewise a `make test` prerequisite and a CI
    # step, #8186) runs it -- and it carries the ratchet's own baseline, so a
    # change to it must never skip the suite that enforces it.
    scripts/check-panic-surface.py) return 1 ;;
    scripts/*.py) return 0 ;;
    LICENSE) return 0 ;;
    */*) return 1 ;;          # any other nested path: not documentation
    *.md|*.tsv|*.svg) return 0 ;;   # top-level records only
    *) return 1 ;;
  esac
}

# Guard: nothing the build reads may be classified as documentation.
#
# Every entry in `is_doc_path` is a claim about *consumers* -- "no job in
# ci.yml reads this tree". Such a claim rots silently and in the dangerous
# direction: wire a new `scripts/*.py` into `make test` the way
# `scripts/migrate-t-layout.py` already is, and a change to it starts reading
# as documentation, skipping the very suite that runs it. Nothing would say so.
#
# So derive the claim instead of trusting it. Every repository path named by
# the Makefile or by ci.yml is a build input by construction; this fails if any
# of them is on the allowlist. Adding a genuinely-new input then costs one
# `return 1` line here rather than a silently-untested merge.
#
# Comment lines are stripped first: both files cite documentation in prose
# (`docs/flaky-test-policy.md`, `docs/adr/0075-...`), and a citation is not an
# input. Paths that do not exist are dropped -- the scan also picks up runner
# paths (`/etc/apt/sources.list`) and `target/<profile>/mutsu` fragments, which
# are noise, not repository files.
#
# Only ONE hop is scanned. A path reached *through* a shell script that CI runs
# is not covered (scanning those too drowns the signal: run-roast-test.sh and
# friends cite docs/ and news/ in running text). When you add that kind of
# indirection, either name the file in the Makefile or ci.yml as well, or deny
# it in `is_doc_path` by hand.
CI_INPUT_SOURCES="Makefile .github/workflows/ci.yml"

check_inputs() {
  local f missing=0
  for f in $CI_INPUT_SOURCES; do
    [ -f "$f" ] || { echo "not ok - $f is not in this checkout" >&2; missing=1; }
  done
  if [ "$missing" -ne 0 ]; then
    echo "ci-docs-only --check-inputs: cannot run without the files above" >&2
    return 1
  fi

  local failures=0 path
  while IFS= read -r path; do
    [ -n "$path" ] || continue
    [ -f "$path" ] || continue
    if is_doc_path "$path"; then
      echo "not ok - $path is read by the build but is on the documentation allowlist" >&2
      failures=$((failures + 1))
    fi
  done <<EOF
$(sed -E 's/^[[:space:]]*#.*$//' $CI_INPUT_SOURCES \
    | grep -ohE '(\.agents|\.claude|\.github|TODO_roast|benchmarks|crates|docs|ecosystem|modules|news|old-design-docs|raku-doc|roast|scripts|site|src|t|tests|tools|vendor)/[A-Za-z0-9_./-]+' \
    | sed -E 's/[.,:;)]+$//' | sort -u)
EOF

  if [ "$failures" -ne 0 ]; then
    echo "ci-docs-only --check-inputs: $failures build input(s) classified as documentation" >&2
    echo "Either deny the path in is_doc_path, or stop reading it from the build." >&2
    return 1
  fi
  echo "ci-docs-only --check-inputs: no build input is on the documentation allowlist"
}

# Reads file paths on stdin, one per line. Empty input => `false`: an empty
# diff means we could not determine what changed, not that nothing changed.
classify() {
  local saw_any=0 path
  while IFS= read -r path; do
    [ -n "$path" ] || continue
    saw_any=1
    if ! is_doc_path "$path"; then
      echo false
      return
    fi
  done
  if [ "$saw_any" -eq 0 ]; then
    echo false
  else
    echo true
  fi
}

# The Miri gate's trigger: does this change touch the GC / container-value code
# whose aliased-write soundness ADR-0013 pins? `src/gc/**` is the collector and
# the `Gc` primitive; `src/value/**` is every container representation that
# takes an aliased in-place write. The workflow and this classifier count too --
# a change to either must re-run the thing it controls.
#
# `src/vm/vm_call_state_guard.rs` is also explicitly listed: it is the ONE file
# outside `src/gc/**`/`src/value/**` that hand-rolls raw-pointer aliasing
# (RAII guards reaching `Interpreter` state across a live call boundary) and
# was the site of a real Stacked-Borrows UB bug that slipped through review
# TWICE because this gate did not cover it -- it was only caught when an
# unrelated `src/value/**` PR happened to trip the gate against the already-
# merged bug. Gate it explicitly so a regression here is never silently
# unverified again.
is_gc_value_path() {
  case "$1" in
    src/gc/*|src/value/*) return 0 ;;
    src/vm/vm_call_state_guard.rs) return 0 ;;
    .github/workflows/ci.yml|scripts/ci-docs-only.sh) return 0 ;;
    *) return 1 ;;
  esac
}

# Reads file paths on stdin. Empty input => `true`: an undeterminable diff must
# run the check, not skip it (the opposite default from `classify`, because the
# consequences are reversed -- see the header).
classify_gc_value() {
  local saw_any=0 path
  while IFS= read -r path; do
    [ -n "$path" ] || continue
    saw_any=1
    if is_gc_value_path "$path"; then
      echo true
      return
    fi
  done
  if [ "$saw_any" -eq 0 ]; then
    echo true
  else
    echo false
  fi
}

changed_files() {
  case "${GITHUB_EVENT_NAME:-}" in
    pull_request)
      # The API list is authoritative for a PR (the checkout is a merge commit,
      # so a local `git diff` against the base is not). --paginate covers PRs
      # larger than one page; the endpoint stops at 3000 files, which
      # `list_is_complete` below detects rather than guesses at.
      gh api --paginate \
        "repos/${GITHUB_REPOSITORY}/pulls/${PR_NUMBER}/files" \
        --jq '.[].filename'
      ;;
    push)
      # `github.event.before` is all-zeroes for a branch's first push and
      # unreachable after a force-push; both print nothing here, and an empty
      # list classifies as `false`.
      case "${GITHUB_EVENT_BEFORE:-}" in
        ''|0000000000000000000000000000000000000000) return 0 ;;
      esac
      gh api --paginate \
        "repos/${GITHUB_REPOSITORY}/compare/${GITHUB_EVENT_BEFORE}...${GITHUB_SHA}" \
        --jq '.files[]?.filename'
      ;;
  esac
}

# How many files the diff has according to the API, for the one event where that
# is knowable exactly. Empty when it is not.
api_file_count() {
  case "${GITHUB_EVENT_NAME:-}" in
    pull_request)
      gh api "repos/${GITHUB_REPOSITORY}/pulls/${PR_NUMBER}" --jq '.changed_files'
      ;;
  esac
}

# Did we read the WHOLE diff? Only a complete list may be classified, because
# truncation hides files and the hidden one could be `src/vm/vm.rs`.
#
# This replaces a count threshold (`over 300 files => refuse to classify`) that
# was a guess standing in for the question, and a bad one: the pull-request
# endpoint serves 3000 files, so the threshold refused ten times more diffs than
# could possibly be short, and every full-corpus `ecosystem/` sweep -- ~1300
# records, all of them on the allowlist, produced by a workflow that measures
# mutsu and cannot change it -- paid for five build jobs to confirm that
# recording what mutsu did does not change what mutsu does.
#
# So ask instead of guessing. A pull request states its own `changed_files`, so
# completeness is an equality, exact at any size: 1269 read of 1269 is complete
# and 3000 read of 4200 is not. A push has no such number to compare against --
# the compare endpoint answers with at most 300 files and does not say how many
# it left out -- so there, and only there, a count is still the whole signal:
# under 300 nothing was dropped, at 300 something may have been.
list_is_complete() { # list_is_complete <event> <api count> <count we read>
  local event="$1" api="$2" seen="$3"
  case "$seen" in ''|*[!0-9]*) echo false; return ;; esac
  case "$event" in
    pull_request)
      case "$api" in ''|*[!0-9]*) echo false; return ;; esac
      if [ "$api" -eq "$seen" ]; then echo true; else echo false; fi
      ;;
    push)
      if [ "$seen" -lt 300 ]; then echo true; else echo false; fi
      ;;
    # An event we do not know how to read a diff from is not a complete diff.
    *) echo false ;;
  esac
}

self_test() {
  local failures=0
  check() { # check <expected> <label> <files...>
    local expected="$1" label="$2"; shift 2
    local got
    got=$(printf '%s\n' "$@" | classify)
    if [ "$got" != "$expected" ]; then
      echo "not ok - $label (expected $expected, got $got)" >&2
      failures=$((failures + 1))
    else
      echo "ok - $label"
    fi
  }

  check true  'adr + news entry'        docs/adr/0016-x.md news/2026-07/y.md
  check false 'retired todo directory'  todo/tickets/z.md
  check true  'top-level plan'          PLAN.md
  check true  'roast ledger'            TODO_roast/BLOCKERS.md
  check true  'vendored docs'           raku-doc/doc/Type/Str.rakudoc
  check true  'non-md under docs/'      docs/probes/pool-spawn.raku
  check true  'agent skill'             .claude/skills/rustc-too-old/SKILL.md
  check true  'agent settings'          .claude/settings.json
  check true  'repo skill'              .agents/skills/cut-release/SKILL.md
  check true  'repo skill helper'       .agents/skills/install-raku/install-raku.sh
  check false 'repo skill + src'        .agents/skills/x/SKILL.md src/vm/vm.rs
  check true  'skill + news entry'      .claude/skills/x/SKILL.md news/2026-09/y.md
  check false 'skill + src'             .claude/skills/x/SKILL.md src/vm/vm.rs
  check false 'src change'              src/vm/vm.rs
  check false 'docs + src'              docs/adr/0016-x.md src/vm/vm.rs
  check false 'workflow change'         .github/workflows/ci.yml
  check false 'test change'             t/regex.t
  check false 'roast whitelist'         roast-whitelist.txt
  check false 'script change'           scripts/run-roast-test.sh
  check false 'nested README'           modules/YAMLish/README.md
  check false 'Cargo manifest'          Cargo.toml
  check false 'empty diff'              ''
  check true  'ecosystem record'        ecosystem/dists/S/String--Utils.json
  check true  'ecosystem sweep'         ecosystem/dists/B/BTree.json ecosystem/history.tsv ecosystem/history.svg
  check false 'ecosystem + its site projection' ecosystem/dists/B/BTree.json site/content/ecosystem.json
  check false 'ecosystem + src'         ecosystem/dists/B/BTree.json src/vm/vm.rs
  check true  'issue template'          .github/ISSUE_TEMPLATE/ticket.md
  check true  'another workflow'        .github/workflows/pages.yml
  check true  'release-note config'     .github/release.yml
  check false 'ci.yml itself'           .github/workflows/ci.yml
  check false 'ci.yml among workflows'  .github/workflows/pages.yml .github/workflows/ci.yml
  check true  'roast history record'    HISTORY.tsv HISTORY-pass.svg
  check true  'reporting python'        scripts/plot_roast_history.py
  check true  'ecosystem tooling'       scripts/ecosystem-sweep.py scripts/ecosystem_common.py
  check false 'a python make test runs' scripts/migrate-t-layout.py
  check false 'the panic ratchet'       scripts/check-panic-surface.py
  check false 'shell script'            scripts/run-t-test.sh
  check false 'node script'             scripts/check-site-snippets.mjs
  check false 'nested tsv'              t/fixtures/data.tsv

  check_complete() { # check_complete <expected> <label> <event> <api count> <read count>
    local expected="$1" label="$2"; shift 2
    local got
    got=$(list_is_complete "$@")
    if [ "$got" != "$expected" ]; then
      echo "not ok - complete/$label (expected $expected, got $got)" >&2
      failures=$((failures + 1))
    else
      echo "ok - complete/$label"
    fi
  }

  # A pull request states its own file count, so completeness is exact at any
  # size -- including the corpus-sweep data PR, which the retired 300-file
  # threshold refused for no reason.
  check_complete true  'pr: counts agree'        pull_request 1269 1269
  check_complete true  'pr: small diff'          pull_request 3 3
  check_complete true  'pr: empty diff'          pull_request 0 0
  check_complete false 'pr: list stops at 3000'  pull_request 4200 3000
  check_complete false 'pr: we read too many'    pull_request 1268 1269
  check_complete false 'pr: no count from api'   pull_request '' 5
  check_complete false 'pr: api count garbage'   pull_request 'null' 5
  # A push has no such number, so the compare endpoint's own 300-file ceiling is
  # the only thing that can be asked.
  check_complete true  'push: under the ceiling' push '' 299
  check_complete false 'push: at the ceiling'    push '' 300
  check_complete false 'push: over the ceiling'  push '' 301
  check_complete false 'unknown event'           schedule '' 4
  check_complete false 'unreadable count'        pull_request 5 ''

  check_gc() { # check_gc <expected> <label> <files...>
    local expected="$1" label="$2"; shift 2
    local got
    got=$(printf '%s\n' "$@" | classify_gc_value)
    if [ "$got" != "$expected" ]; then
      echo "not ok - gc/$label (expected $expected, got $got)" >&2
      failures=$((failures + 1))
    else
      echo "ok - gc/$label"
    fi
  }

  check_gc true  'gc primitive'          src/gc/gc_ptr.rs
  check_gc true  'container value'       src/value/aliased_mut.rs
  check_gc true  'gc among others'       src/vm/vm.rs src/gc/collect.rs
  check_gc true  'the workflow itself'   .github/workflows/ci.yml
  check_gc true  'this classifier'       scripts/ci-docs-only.sh
  check_gc true  'call-state-guard file' src/vm/vm_call_state_guard.rs
  check_gc true  'empty diff'            ''
  check_gc false 'unrelated src'         src/vm/vm.rs src/parser/mod.rs
  check_gc false 'docs only'             PLAN.md docs/adr/0013-x.md
  check_gc false 'value in another tree' modules/URI/lib/value.rakumod

  # The allowlist cases above only pin what the rules *say*. Run the guard too
  # whenever the checkout has the files it reads, so a local run catches a
  # build input that drifted onto the allowlist; CI calls it as its own step so
  # a sparse checkout that omits them fails loudly instead of skipping it.
  local f have_sources=1
  for f in $CI_INPUT_SOURCES; do
    [ -f "$f" ] || have_sources=0
  done
  if [ "$have_sources" -eq 1 ]; then
    check_inputs || failures=$((failures + 1))
  else
    echo "ok - (skipping --check-inputs: $CI_INPUT_SOURCES not in this checkout)"
  fi

  if [ "$failures" -ne 0 ]; then
    echo "ci-docs-only self-test: $failures failure(s)" >&2
    return 1
  fi
  echo "ci-docs-only self-test: all cases pass"
}

case "${1:-}" in
  --self-test)          self_test; exit $? ;;
  --check-inputs)       check_inputs; exit $? ;;
  --classify)           classify; exit 0 ;;
  --classify-gc-value)  classify_gc_value; exit 0 ;;
esac

files=$(changed_files 2>/dev/null)
count=$(printf '%s\n' "$files" | grep -c .)
complete=$(list_is_complete "${GITHUB_EVENT_NAME:-}" "$(api_file_count 2>/dev/null)" "$count")

if [ "${1:-}" = "--gc-value" ]; then
  # A diff we could not read in full must read as "run it" here, the opposite of
  # the docs-only default below.
  if [ "$complete" != true ]; then
    echo true
    exit 0
  fi
  printf '%s\n' "$files" | classify_gc_value
  exit 0
fi

# A diff we could not read in full must never read as "docs only".
if [ "$complete" != true ]; then
  echo false
  exit 0
fi
printf '%s\n' "$files" | classify
