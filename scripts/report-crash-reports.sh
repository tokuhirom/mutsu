#!/bin/bash
# Surface any crash reports a CI job's mutsu processes left behind.
#
# `src/crash_report/` installs a fatal-signal handler that writes
# tmp/crash/<pid>.txt naming the signal, fault address, pid, argv and a
# backtrace, then lets the signal through unchanged. Without this step those
# files would only exist inside the runner: a `Wstat: 11 (Signal: SEGV)` line
# in a prove summary says nothing about *which* process faulted (the
# interpreter running the .t file, or a subprocess `is_run` spawned) or where.
#
# A crash rare enough to appear once in several dozen CI runs has to yield its
# evidence the one time it fires, so this prints every report into the job log
# in addition to the artifact upload alongside it. This step now runs with
# `if: always()` (not just `if: failure()`), because a crash can happen inside
# a subprocess a test deliberately provokes (see the allowlist below) while
# the job that spawned it still goes green overall -- that combination is
# exactly what buried the genuine `advent2014-day05.t` heap-corruption crash
# for 19 days (todo/deep/procasync-stress-segv.md). So this script now FAILS
# the job itself when it finds a report whose `argv:` is not on the
# allowlist, instead of unconditionally exiting 0 -- a fatal signal outside a
# handful of tests that deliberately trigger one is never expected, crash or
# no other failure in the job.

set -uo pipefail
shopt -s nullglob

DIR="${MUTSU_CRASH_DIR:-tmp/crash}"

# Reports whose `argv:` CONTAINS one of these substrings are known,
# deliberate crashes a test provokes on purpose (and asserts the resulting
# behavior on) -- not evidence of a real bug. Keep this list small and add an
# entry only when a specific test is documented to crash a subprocess on
# purpose.
ALLOWLISTED_ARGV_SUBSTRINGS=(
  # roast/S29-os/system.t: "Exit with a segfault makes the Proc throw in
  # sink context" -- rakudo#3149. Deliberately calls strdup(0) via
  # NativeCall to segfault a spawned mutsu subprocess and asserts that the
  # parent's `run()` reports it as X::Proc::Unsuccessful.
  "-e use NativeCall; sub strdup(int64) is native(Str) {*}; strdup(0)"
)

reports=("$DIR"/*.txt)
if [ ${#reports[@]} -eq 0 ]; then
  echo "No crash reports: nothing died of a fatal signal in this job."
  exit 0
fi

echo "::error::${#reports[@]} mutsu process(es) died of a fatal signal. Reports below and in the crash-reports artifact."

unexpected=0
for f in "${reports[@]}"; do
  echo "::group::$f"
  cat "$f"
  echo "::endgroup::"

  argv=$(sed -n 's/^argv: //p' "$f" | head -1)
  allowlisted=0
  for needle in "${ALLOWLISTED_ARGV_SUBSTRINGS[@]}"; do
    case "$argv" in
      *"$needle"*) allowlisted=1; break ;;
    esac
  done
  if [ "$allowlisted" -eq 1 ]; then
    echo "  -> known deliberate crash (argv matches the allowlist), not treated as a failure."
  else
    echo "::error::$(basename "$f") is NOT on the allowlist -- treating as a real, unexpected crash."
    unexpected=$((unexpected + 1))
  fi
done

if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
  {
    echo "### Fatal-signal crash reports"
    echo
    echo "| report | signal | fault address | argv |"
    echo "| --- | --- | --- | --- |"
    for f in "${reports[@]}"; do
      sig=$(sed -n 's/^signal: //p' "$f" | head -1)
      addr=$(sed -n 's/^fault-addr: //p' "$f" | head -1)
      argv=$(sed -n 's/^argv: //p' "$f" | head -1)
      echo "| \`$(basename "$f")\` | ${sig:-?} | \`${addr:-?}\` | \`${argv:-?}\` |"
    done
    echo
    echo "Full reports are in the job log above and the \`crash-reports\` artifact."
    echo "Release builds carry no line tables, so resolve raw frames with"
    echo "\`addr2line -f -e target/release/mutsu <address>\`."
    if [ "$unexpected" -gt 0 ]; then
      echo
      echo "**$unexpected report(s) are NOT on the allowlist in this script and were"
      echo "treated as a real failure.** If a report is actually a known, deliberate"
      echo "crash a test provokes on purpose, add its \`argv:\` substring to"
      echo "\`ALLOWLISTED_ARGV_SUBSTRINGS\` in \`scripts/report-crash-reports.sh\`."
    fi
  } >> "$GITHUB_STEP_SUMMARY"
fi

if [ "$unexpected" -gt 0 ]; then
  echo "::error::$unexpected crash report(s) are not on the allowlist -- failing this step."
  exit 1
fi

echo "All ${#reports[@]} crash report(s) match the allowlist (deliberate, expected crashes) -- not failing the job."
exit 0
