#!/bin/bash
# Surface the quarantine retries a CI job consumed.
#
# `scripts/flaky-retry.sh` appends one line per failed attempt to
# tmp/flaky-retries.log. This turns that into a GitHub warning annotation and a
# job-summary table, because a quarantine that nobody sees is a deleted test.
# A rising retry count on an entry is the signal to go re-measure it.
#
# Always exits 0: retries are information, not a failure. A quarantined test
# that exhausts its attempts has already failed the suite by itself.

set -uo pipefail

LOG="${FLAKY_RETRY_LOG:-tmp/flaky-retries.log}"

if [ ! -s "$LOG" ]; then
  echo "No quarantine retries were needed in this job."
  exit 0
fi

total=$(wc -l < "$LOG")
echo "::warning::$total quarantined-test retry attempt(s) were consumed in this job. See the job summary."

summary=$(sort "$LOG" | awk '{print $1}' | uniq -c | sort -rn)

echo "$summary" | while read -r count test; do
  echo "  $test: $count failed attempt(s)"
done

if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
  {
    echo "### Flaky quarantine retries"
    echo
    echo "| test | failed attempts |"
    echo "| --- | --- |"
    echo "$summary" | while read -r count test; do
      echo "| \`$test\` | $count |"
    done
    echo
    echo "Retries come from \`flaky-tests.txt\`. If a count here is climbing,"
    echo "re-measure the entry (\`scripts/flake-repro.sh\`) — see"
    echo "\`docs/flaky-test-policy.md\`."
  } >> "$GITHUB_STEP_SUMMARY"
fi
