#!/bin/bash
# Mine the GitHub Actions history to measure which tests ACTUALLY fail
# intermittently, so the flaky quarantine list (flaky-tests.txt) is backed by
# evidence instead of folklore.
#
# Usage:
#   scripts/ci-flake-survey.sh [run-count]        # default 200 most recent CI runs
#
# Output (stdout): a report with two tables
#   1. per-test failure tally, split by "job spread"
#   2. the quarantine-relevant subset (single-job and/or push:main failures)
#
# Why the job spread matters: every PR runs the same suite three times, in the
# `test`, `gc-stress` and `jit-stress` jobs. A genuine regression fails in all
# three (the code is broken in every configuration). A test that fails in only
# ONE of the three, with the other two green on the same commit, is by
# construction non-deterministic -- the binary and the inputs were identical.
# A failure on a `push: main` run is an even stronger signal: main is protected,
# so that exact tree already passed the full suite on its PR minutes earlier.
#
# Raw per-failure rows are written to tmp/ci-flake-survey.tsv for follow-up.
# Job logs are cached under tmp/ci-flake-logs/ so re-runs are cheap.

set -uo pipefail

RUNS="${1:-200}"
REPO="${REPO:-tokuhirom/mutsu}"
LOGDIR="tmp/ci-flake-logs"
ROWS="tmp/ci-flake-survey.tsv"

mkdir -p "$LOGDIR"
: > "$ROWS"

echo "Surveying the last $RUNS CI runs of $REPO ..." >&2

gh run list --repo "$REPO" --workflow ci.yml --limit "$RUNS" \
  --json databaseId,headBranch,conclusion,event,createdAt \
  -q '.[] | [.databaseId, .conclusion, .event, .headBranch, .createdAt] | @tsv' \
  > tmp/ci-flake-runs.tsv

total_runs=$(wc -l < tmp/ci-flake-runs.tsv)
failed_runs=$(awk -F'\t' '$2=="failure"' tmp/ci-flake-runs.tsv | wc -l)
echo "  $total_runs runs listed, $failed_runs concluded 'failure'" >&2

# For every failed run, pull the log of each failed job and extract the tests
# that actually failed. `gh run view --log-failed` is unreliable on large jobs
# (it silently yields nothing), so go through the REST API per job instead.
awk -F'\t' '$2=="failure" {print $1"\t"$3"\t"$4}' tmp/ci-flake-runs.tsv |
while IFS=$'\t' read -r run_id event branch; do
  jobs=$(gh api "repos/$REPO/actions/runs/$run_id/jobs" --paginate \
    -q '.jobs[] | select(.conclusion=="failure") | [(.id|tostring), .name] | @tsv' 2>/dev/null)
  [ -z "$jobs" ] && continue
  while IFS=$'\t' read -r job_id job_name; do
    log="$LOGDIR/$job_id.log"
    if [ ! -s "$log" ]; then
      gh api "repos/$REPO/actions/jobs/$job_id/logs" > "$log" 2>/dev/null
    fi
    [ -s "$log" ] || continue

    # prove's Test Summary Report lists every failed file. Skip the entries
    # that carry "Wstat: 0 ... Failed: 0" -- prove also lists files whose only
    # anomaly is an unexpectedly-passing TODO, which is not a failure.
    grep -E '\(Wstat' "$log" |
      grep -vE 'Wstat: 0 .*Failed: 0\)' |
      sed -E 's/^[0-9T:.Z-]+ //; s/[[:space:]]+\(Wstat.*//' |
      sort -u |
      while read -r test_file; do
        printf '%s\t%s\t%s\t%s\ttest\t%s\n' "$run_id" "$event" "$branch" "$job_name" "$test_file"
      done

    # cargo test failures show up as "---- <path::to::test> stdout ----".
    grep -oE -- '---- [A-Za-z0-9_:]+ stdout ----' "$log" |
      sed -E 's/^---- //; s/ stdout ----$//' | sort -u |
      while read -r unit; do
        printf '%s\t%s\t%s\t%s\tunit\t%s\n' "$run_id" "$event" "$branch" "$job_name" "$unit"
      done

    # Steps that fail without producing a test name at all (fmt, clippy, build).
    if ! grep -qE '\(Wstat|---- [A-Za-z0-9_:]+ stdout ----' "$log"; then
      printf '%s\t%s\t%s\t%s\tstep\t(no test-level failure found)\n' \
        "$run_id" "$event" "$branch" "$job_name"
    fi
  done <<< "$jobs"
done >> "$ROWS"

echo >&2
echo "Raw rows: $ROWS ($(wc -l < "$ROWS") failure records)" >&2
echo >&2

awk -F'\t' '
{
  key = $6
  runs[key "\x1f" $1] = 1
  jobs[key "\x1f" $1 "\x1f" $4] = 1
  if ($2 == "push" && $3 == "main") mainfail[key]++
  seen[key] = 1
}
END {
  # For each (test, run) pair count how many of the three jobs saw the failure.
  for (rk in runs) {
    split(rk, a, "\x1f"); key = a[1]; run = a[2]
    n = 0
    for (jk in jobs) {
      split(jk, b, "\x1f")
      if (b[1] == key && b[2] == run) n++
    }
    occurrences[key]++
    if (n == 1) single[key]++
    else broad[key]++
  }
  printf "%-58s %6s %8s %7s %9s\n", "TEST", "RUNS", "1-JOB", "N-JOB", "MAIN-PUSH"
  printf "%-58s %6s %8s %7s %9s\n", "----", "----", "-----", "-----", "---------"
  for (k in seen) {
    printf "%-58s %6d %8d %7d %9d\n", k, occurrences[k], single[k]+0, broad[k]+0, mainfail[k]+0
  }
}' "$ROWS" | { read -r h1; read -r h2; echo "$h1"; echo "$h2"; sort -k3 -rn -k2 -rn; }

echo
echo "Legend:"
echo "  1-JOB     failed in exactly one of test/gc-stress/jit-stress on that run"
echo "            -> same binary, same inputs, different verdict = non-deterministic"
echo "  N-JOB     failed in several jobs of the same run -> almost always a real regression"
echo "  MAIN-PUSH failed on a push to main, i.e. on a tree that had just passed CI"
echo
echo "Quarantine candidates are rows with a high 1-JOB or MAIN-PUSH count and a"
echo "low N-JOB count. Confirm each one with scripts/flake-repro.sh before adding"
echo "it to flaky-tests.txt."
