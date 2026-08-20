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
    echo "$?" > "$WORK/$name.native.st"
    ( cd "$WORK" && MUTSU_REAL_TEST=1 timeout 90 "$MUTSU" -I "$ROOT/t/lib" "t/$name" \
        > "$WORK/$name.real" 2>&1 )
    echo "$?" > "$WORK/$name.real.st"
    # A failing file exits non-zero, and a short plan exits 255 -- which xargs
    # would treat as "abort the whole run" if that status propagated out of
    # run_one. So run_one itself always returns 0; the two ".st" files carry
    # the real exit status forward for the classification pass below.
    return 0
}
export -f run_one
export ROOT WORK MUTSU

ls t/*.t | xargs -P "$JOBS" -I{} bash -c 'run_one "$@"' _ {}

# A file passes only if it (a) exited 0 and (b) printed no failure marker.
# Exit status matters on its own: a mid-file abort under the real Test
# module's END-phaser plan check prints *only*
# "# You planned N tests, but ran M" and exits 255 -- no "not ok", no
# "Runtime error" -- so the text-only grep used to score that as a pass.
passes() {
    local out="$1" st="$2"
    [ "$(cat "$st" 2>/dev/null)" = "0" ] || return 1
    ! grep -qE '^(not ok|Runtime error|Parse error|===SORRY)' "$out" \
        && ! grep -q '^# You planned' "$out"
}

both=0 regressed=0 real_only=0 neither=0
: > "$WORK/regressions.txt"
for n in "$WORK"/*.native; do
    name="$(basename "$n" .native)"
    r="$WORK/$name.real"
    if passes "$n" "$WORK/$name.native.st" && passes "$r" "$WORK/$name.real.st"; then both=$((both + 1))
    elif passes "$n" "$WORK/$name.native.st"; then
        regressed=$((regressed + 1))
        {
            echo "=== $name (exit $(cat "$WORK/$name.real.st" 2>/dev/null))"
            grep -m4 -E '^(not ok|Runtime error|Parse error|===SORRY|# Failed|# You planned)' "$r"
        } >> "$WORK/regressions.txt"
    elif passes "$r" "$WORK/$name.real.st"; then real_only=$((real_only + 1))
    else neither=$((neither + 1))
    fi
done

echo "pass under both:                   $both"
echo "regressed under the real Test:     $regressed"
echo "passes only under the real Test:   $real_only"
echo "fail under both (pre-existing):    $neither"
echo "regression detail: $WORK/regressions.txt"
