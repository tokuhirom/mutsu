#!/bin/bash
# The roast-side counterpart of scripts/test-module-sweep.sh: run every
# WHITELISTED roast file twice -- once with mutsu's native TAP provider and once
# with the vendored upstream Test.rakumod (MUTSU_REAL_TEST=1) -- and report which
# files regress under the real module.
#
# This is the measurement the roast half of
# `todo/deep/vendor-real-test-module.md` runs on. That ticket's process note
# asks for a fresh sweep at the start of every session that touches it, because
# `MUTSU_REAL_TEST` is not gated in CI and nothing else detects a regression in
# this mode.
#
# Differences from the t/ sweep, all deliberate:
#   * Files run IN PLACE from the repo root through scripts/run-roast-test.sh,
#     so they inherit its per-file timeouts, its `MUTSU_FUDGE=1` export (roast
#     needs it) and its roast/-cwd special cases. There is no working copy, so
#     the t/ sweep's cwd-artifact class cannot occur here.
#   * A RELEASE build by default: the whitelist is ~1400 files and each is run
#     twice.
#
# Note on timeouts: the real module answers every assertion through Raku-level
# code, so assertion-heavy files (the S32-str/sprintf-* and S03-buf/*int
# families) take several times longer than under the native provider and can
# exceed run-roast-test.sh's budget. An `exit 124` row in the report is a
# PERFORMANCE artifact of the real provider, not a correctness regression --
# re-run the file with a larger budget before counting it.
#
# Usage: scripts/roast-test-module-sweep.sh [jobs]     (default 6)
set -u
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT" || exit 1
JOBS="${1:-6}"
export MUTSU_BIN="${MUTSU_BIN:-$ROOT/target/release/mutsu}"
WORK="$ROOT/tmp/roast-real-sweep"

rm -rf "$WORK"
mkdir -p "$WORK/out"
# Same hygiene `make roast` applies: a stale copy left by an interrupted
# S32-io/spurt.t makes that test refuse to run.
rm -f "$ROOT/temp-file-RT-126006-test"

run_one() {
    local f="$1" name
    name="$(echo "$f" | tr '/' '_')"
    # The sweep measures Test-provider semantics.  Disable the optional module
    # precomp cache so an unwritable cache directory in a child `is_run` process
    # cannot turn into unexpected stderr under the real Raku Test module.
    ( cd "$ROOT" && MUTSU_PRECOMP=0 MUTSU_REAL_TEST= ./scripts/run-roast-test.sh "$f" \
        > "$WORK/out/$name.native" 2>&1 )
    echo "$?" > "$WORK/out/$name.native.st"
    ( cd "$ROOT" && MUTSU_PRECOMP=0 MUTSU_REAL_TEST=1 ./scripts/run-roast-test.sh "$f" \
        > "$WORK/out/$name.real" 2>&1 )
    echo "$?" > "$WORK/out/$name.real.st"
    echo "$f" > "$WORK/out/$name.path"
    # Never propagate the test's status: a non-zero exit would make xargs
    # abandon the rest of the sweep. The ".st" files carry it forward instead.
    return 0
}
export -f run_one
export ROOT WORK

grep -v '^#' "$ROOT/roast-whitelist.txt" | grep -v '^[[:space:]]*$' \
  | xargs -P "$JOBS" -I{} bash -c 'run_one "$@"' _ {}

# Identical predicate to scripts/test-module-sweep.sh: exit status 0 AND no
# failure marker, with a TAP `# TODO`-annotated `not ok` treated as the expected
# failure it is.
passes() {
    local out="$1" st="$2"
    [ "$(cat "$st" 2>/dev/null)" = "0" ] || return 1
    grep -qaE '^(Runtime error|Parse error|===SORRY)' "$out" && return 1
    grep -qa '^# You planned' "$out" && return 1
    grep -aE '^not ok' "$out" | grep -qvi '#[[:space:]]*todo' && return 1
    return 0
}

both=0 regressed=0 real_only=0 neither=0
: > "$WORK/regressions.txt"
: > "$WORK/regressed-files.txt"
: > "$WORK/fail-both.txt"
for n in "$WORK"/out/*.native; do
    name="$(basename "$n" .native)"
    r="$WORK/out/$name.real"
    path="$(cat "$WORK/out/$name.path" 2>/dev/null)"
    if passes "$n" "$WORK/out/$name.native.st" && passes "$r" "$WORK/out/$name.real.st"; then
        both=$((both + 1))
    elif passes "$n" "$WORK/out/$name.native.st"; then
        regressed=$((regressed + 1))
        echo "$path" >> "$WORK/regressed-files.txt"
        {
            echo "=== $path (exit $(cat "$WORK/out/$name.real.st" 2>/dev/null))"
            grep -m4 -aE '^(not ok|Runtime error|Parse error|===SORRY|# Failed|# You planned)' "$r"
        } >> "$WORK/regressions.txt"
    elif passes "$r" "$WORK/out/$name.real.st"; then
        real_only=$((real_only + 1))
    else
        neither=$((neither + 1))
        echo "$path" >> "$WORK/fail-both.txt"
    fi
done

echo "pass under both:                   $both"
echo "regressed under the real Test:     $regressed"
echo "passes only under the real Test:   $real_only"
echo "fail under both (pre-existing):    $neither"
echo "regression detail: $WORK/regressions.txt"
echo "regressed file list: $WORK/regressed-files.txt"
