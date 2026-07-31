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
# in addition to the artifact upload alongside it.
#
# Always exits 0: the crash itself has already failed the job that produced it.

set -uo pipefail
shopt -s nullglob

DIR="${MUTSU_CRASH_DIR:-tmp/crash}"

reports=("$DIR"/*.txt)
if [ ${#reports[@]} -eq 0 ]; then
  echo "No crash reports: nothing died of a fatal signal in this job."
  exit 0
fi

echo "::error::${#reports[@]} mutsu process(es) died of a fatal signal. Reports below and in the crash-reports artifact."

for f in "${reports[@]}"; do
  echo "::group::$f"
  cat "$f"
  echo "::endgroup::"
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
  } >> "$GITHUB_STEP_SUMMARY"
fi
