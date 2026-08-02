#!/bin/bash
# Run every t/*.t twice -- once with mutsu's native TAP provider and once with
# the vendored upstream Test.rakumod (MUTSU_REAL_TEST=1) -- and report which
# files regress under the real module.
#
# This is the measurement that drives step 2 of
# `todo/tickets/vendor-real-test-module.md`. It replaces the throwaway
# `unit module Test2;` rename the exercise ran under before: the file under
# test is now the unmodified upstream one vendored at
# `modules/Rakudo-Core/lib/Test.rakumod`.
#
# The two runs are deliberately NOT compared byte-for-byte. The real module is
# routinely *more* faithful than the native provider (richer `throws-like`
# subtests, `'<code>' died` descriptions instead of `code dies`), so the
# question is whether a file that passed still passes.
#
# Usage: scripts/test-module-sweep.sh [jobs]     (default 4)
set -u
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT" || exit 1
JOBS="${1:-4}"
MUTSU="${MUTSU_BIN:-$ROOT/target/debug/mutsu}"
WORK="$ROOT/tmp/test-module-sweep"

rm -rf "$WORK"
mkdir -p "$WORK/t"
# The test files reach for their fixtures as 't/lib/...', relative to cwd, so
# the copies live under a directory that keeps the same shape.
ln -sfn "$ROOT/t/lib" "$WORK/t/lib"

run_one() {
    local f="$1" name
    name="$(basename "$f")"
    cp "$f" "$WORK/t/$name"
    ( cd "$WORK" && MUTSU_REAL_TEST= timeout 90 "$MUTSU" -I "$ROOT/t/lib" "t/$name" \
        > "$WORK/$name.native" 2>&1 )
    ( cd "$WORK" && MUTSU_REAL_TEST=1 timeout 90 "$MUTSU" -I "$ROOT/t/lib" "t/$name" \
        > "$WORK/$name.real" 2>&1 )
    # A failing file exits non-zero, and a short plan exits 255 -- which xargs
    # treats as "abort the whole run". The exit status is not the signal here;
    # the captured output is.
    return 0
}
export -f run_one
export ROOT WORK MUTSU

ls t/*.t | xargs -P "$JOBS" -I{} bash -c 'run_one "$@"' _ {}

passes() { ! grep -qE '^(not ok|Runtime error|Parse error|===SORRY)' "$1"; }

both=0 regressed=0 real_only=0 neither=0
: > "$WORK/regressions.txt"
for n in "$WORK"/*.native; do
    name="$(basename "$n" .native)"
    r="$WORK/$name.real"
    if passes "$n" && passes "$r"; then both=$((both + 1))
    elif passes "$n"; then
        regressed=$((regressed + 1))
        {
            echo "=== $name"
            grep -m4 -E '^(not ok|Runtime error|Parse error|===SORRY|# Failed)' "$r"
        } >> "$WORK/regressions.txt"
    elif passes "$r"; then real_only=$((real_only + 1))
    else neither=$((neither + 1))
    fi
done

echo "pass under both:                   $both"
echo "regressed under the real Test:     $regressed"
echo "passes only under the real Test:   $real_only"
echo "fail under both (pre-existing):    $neither"
echo "regression detail: $WORK/regressions.txt"
