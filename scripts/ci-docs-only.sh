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
#   printf 'a\nb\n' | scripts/ci-docs-only.sh --classify   # classify a list

set -u

# A path is documentation iff it matches one of these. Everything else --
# src/, t/, roast/, scripts/, site/, modules/, vendor/, benchmarks/,
# Cargo.*, Makefile, roast-whitelist.txt, flaky-tests.txt, and crucially
# .github/** itself -- forces the full suite.
#
# Deliberately NOT `**/*.md`: a README under modules/ or site/ sits next
# to files the build reads, and the blast radius of guessing wrong there is a
# silently-untested merge. Top-level *.md (PLAN, README, CLAUDE, ANALYSIS,
# PERFORMANCE, BATTERIES, AGENTS) is safe and covers the common case.
is_doc_path() {
  case "$1" in
    docs/*|news/*|todo/*|TODO_roast/*|old-design-docs/*|raku-doc/*) return 0 ;;
    LICENSE) return 0 ;;
    */*) return 1 ;;          # any other nested path: not documentation
    *.md) return 0 ;;         # top-level markdown only
    *) return 1 ;;
  esac
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

changed_files() {
  case "${GITHUB_EVENT_NAME:-}" in
    pull_request)
      # The API list is authoritative for a PR (the checkout is a merge commit,
      # so a local `git diff` against the base is not). --paginate covers PRs
      # larger than one page; the 3000-file API cap only matters for diffs far
      # bigger than any docs-only change, and truncation can only add unseen
      # files, which the caller treats as... nothing. So cap it explicitly:
      # a PR over 300 files is classified `false` by the count guard below.
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
  check true  'todo ticket'             todo/tickets/z.md
  check true  'top-level plan'          PLAN.md
  check true  'roast ledger'            TODO_roast/BLOCKERS.md
  check true  'vendored docs'           raku-doc/doc/Type/Str.rakudoc
  check true  'non-md under docs/'      docs/probes/pool-spawn.raku
  check false 'src change'              src/vm/vm.rs
  check false 'docs + src'              docs/adr/0016-x.md src/vm/vm.rs
  check false 'workflow change'         .github/workflows/ci.yml
  check false 'test change'             t/regex.t
  check false 'roast whitelist'         roast-whitelist.txt
  check false 'script change'           scripts/run-roast-test.sh
  check false 'nested README'           modules/YAMLish/README.md
  check false 'Cargo manifest'          Cargo.toml
  check false 'empty diff'              ''

  if [ "$failures" -ne 0 ]; then
    echo "ci-docs-only self-test: $failures failure(s)" >&2
    return 1
  fi
  echo "ci-docs-only self-test: all cases pass"
}

case "${1:-}" in
  --self-test) self_test; exit $? ;;
  --classify)  classify; exit 0 ;;
esac

files=$(changed_files 2>/dev/null)
# Guard against a huge diff: pagination or the API cap could truncate it, and a
# truncated list must never read as "docs only".
if [ "$(printf '%s\n' "$files" | grep -c .)" -gt 300 ]; then
  echo false
  exit 0
fi
printf '%s\n' "$files" | classify
