#!/bin/bash
# Run one test file, retrying it if -- and ONLY if -- it is listed in the
# quarantine ledger `flaky-tests.txt`.
#
# Usage: scripts/flaky-retry.sh <test-file> <command> [args...]
#
# The command is expected to emit TAP on stdout. On success the output is passed
# through unchanged. For a quarantined file that fails, the failed attempt's
# output is DISCARDED (emitting it would confuse prove with two plans) and the
# command runs again, up to $FLAKY_MAX_ATTEMPTS times. The first attempt that
# passes is what prove sees, preceded by a `# flaky-retry:` TAP comment so the
# retry is visible in the log rather than silent.
#
# A file that is NOT in the ledger is never retried: a single run, streamed
# straight through. So quarantine is an explicit, reviewable decision, and a
# genuine regression in an unlisted test still fails on the first attempt.
#
# A quarantined file that fails EVERY attempt still fails the build. Quarantine
# buys a re-roll of the dice, not immunity: a deterministic regression -- even
# inside a quarantined file -- fails all attempts and blocks the merge.
#
# Every retry is appended to $FLAKY_RETRY_LOG (default tmp/flaky-retries.log) so
# CI can surface the tally. Silent quarantine is how a "known flaky" list rots;
# see docs/flaky-test-policy.md.

set -uo pipefail

FLAKY_LIST="${FLAKY_LIST:-flaky-tests.txt}"
FLAKY_MAX_ATTEMPTS="${FLAKY_MAX_ATTEMPTS:-3}"
FLAKY_RETRY_LOG="${FLAKY_RETRY_LOG:-tmp/flaky-retries.log}"

test_file="$1"
shift

is_quarantined() {
  local want="$1" path _rest
  [ -f "$FLAKY_LIST" ] || return 1
  while read -r path _rest; do
    case "$path" in ''|'#'*) continue ;; esac
    [ "$path" = "$want" ] && return 0
  done < "$FLAKY_LIST"
  return 1
}

if ! is_quarantined "$test_file"; then
  exec "$@"
fi

attempt=1
while :; do
  output=$("$@" 2>&1)
  rc=$?
  if [ $rc -eq 0 ]; then
    [ $attempt -gt 1 ] && printf '# flaky-retry: %s passed on attempt %d/%d\n' \
      "$test_file" "$attempt" "$FLAKY_MAX_ATTEMPTS"
    printf '%s\n' "$output"
    exit 0
  fi
  mkdir -p "$(dirname "$FLAKY_RETRY_LOG")" 2>/dev/null
  printf '%s attempt %d/%d failed (rc=%d)\n' \
    "$test_file" "$attempt" "$FLAKY_MAX_ATTEMPTS" "$rc" >> "$FLAKY_RETRY_LOG"
  if [ "$attempt" -ge "$FLAKY_MAX_ATTEMPTS" ]; then
    # Out of re-rolls: this is a failure like any other. Emit the last
    # attempt's output so the diagnosis is in the log.
    printf '# flaky-retry: %s FAILED all %d attempts -- treating as a real failure\n' \
      "$test_file" "$FLAKY_MAX_ATTEMPTS"
    printf '%s\n' "$output"
    exit $rc
  fi
  attempt=$((attempt + 1))
done
