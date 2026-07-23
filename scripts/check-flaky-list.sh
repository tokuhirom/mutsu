#!/bin/bash
# Validate the flaky quarantine ledger (flaky-tests.txt).
#
# Run by `make check-flaky-list` and by CI. Enforces the parts of
# docs/flaky-test-policy.md a machine can check:
#
#   - the format parses (path, added date, review date, non-empty reason)
#   - the quarantined file actually exists
#   - a quarantined roast file is on the whitelist (otherwise it never runs and
#     the entry is dead weight)
#   - no duplicate entries, list is sorted
#   - the review date is in the future and at most 180 days past `added`
#
# The review deadline is the point of the whole file: a quarantine that nobody
# ever revisits is indistinguishable from deleting the test. When an entry
# expires, CI fails until someone re-measures it (scripts/ci-flake-survey.sh +
# scripts/flake-repro.sh) and either drops the entry or renews it with fresh
# numbers in the reason.

set -uo pipefail

LIST="${1:-flaky-tests.txt}"
TODAY="${FLAKY_TODAY:-$(date -u +%Y-%m-%d)}"

if [ ! -f "$LIST" ]; then
  echo "error: $LIST not found" >&2
  exit 1
fi

status=0
lineno=0
prev_path=""
declare -A seen=()

is_iso_date() { [[ "$1" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]]; }

# Days between two ISO dates, via `date`. Only used for the 180-day cap; the
# expiry comparison itself is a lexical string compare, which ISO dates permit
# and which needs no `date` at all.
days_between() {
  local a b
  a=$(date -u -d "$1" +%s 2>/dev/null) || return 1
  b=$(date -u -d "$2" +%s 2>/dev/null) || return 1
  echo $(((b - a) / 86400))
}

while IFS= read -r line || [ -n "$line" ]; do
  lineno=$((lineno + 1))
  case "$line" in ''|'#'*) continue ;; esac

  read -r path added review reason <<< "$line"

  if [ -z "${reason:-}" ]; then
    echo "$LIST:$lineno: expected '<path> <added> <review> <reason>', got: $line" >&2
    status=1
    continue
  fi

  if [ ! -f "$path" ]; then
    echo "$LIST:$lineno: quarantined file does not exist: $path" >&2
    status=1
  fi

  if [[ "$path" == roast/* ]] && ! grep -qxF "$path" roast-whitelist.txt; then
    echo "$LIST:$lineno: $path is not in roast-whitelist.txt, so it never runs" >&2
    status=1
  fi

  for d in "$added" "$review"; do
    if ! is_iso_date "$d"; then
      echo "$LIST:$lineno: not an ISO date: $d" >&2
      status=1
    fi
  done

  if [ -n "${seen[$path]:-}" ]; then
    echo "$LIST:$lineno: duplicate entry for $path" >&2
    status=1
  fi
  seen[$path]=1

  if [ -n "$prev_path" ] && [[ "$path" < "$prev_path" ]]; then
    echo "$LIST:$lineno: not sorted ($path after $prev_path)" >&2
    status=1
  fi
  prev_path="$path"

  if is_iso_date "$added" && is_iso_date "$review"; then
    if [[ "$review" < "$TODAY" ]]; then
      echo "$LIST:$lineno: quarantine for $path expired on $review." >&2
      echo "    Re-measure it before renewing:" >&2
      echo "      scripts/ci-flake-survey.sh 300" >&2
      echo "      scripts/flake-repro.sh -n 20 -l 6 $path" >&2
      echo "    Then either delete the entry (it is fixed / no longer flaky) or" >&2
      echo "    renew it with the fresh numbers recorded in the reason." >&2
      status=1
    fi
    span=$(days_between "$added" "$review") || span=0
    if [ "$span" -gt 180 ]; then
      echo "$LIST:$lineno: review window for $path is ${span}d; the cap is 180d" >&2
      status=1
    fi
  fi
done < "$LIST"

count=${#seen[@]}
if [ "$count" -gt 10 ]; then
  echo "warning: $count quarantined tests. Above ~10 the suite is drifting from" >&2
  echo "         'a few known-random tests' toward 'CI is advisory'." >&2
fi

if [ $status -eq 0 ]; then
  echo "flaky-tests.txt OK ($count quarantined test(s), all reviewed by $TODAY)"
fi
exit $status
