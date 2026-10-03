#!/usr/bin/env bash
# Shrinking ratchet on name-matching method dispatch arms (ADR-11276).
#
# A built-in method is meant to be one registered row in
# src/builtins/method_table/ -- `(owner, name, arity, handler)` -- reached
# through that row. Until the migration is done, most methods are still
# `match` arms on the method NAME's `&str`:
#
#   pure  src/builtins/methods_0arg/, src/builtins/methods_narg/
#         (`native_method_0arg` / `_1arg` / `_2arg`, no interpreter)
#   slow  src/runtime/methods*.rs and their subdirectories
#         (`call_method_with_values` and the slow-path handlers)
#
# An arm is a line that starts with a quoted method name -- optionally
# `| "other"` alternatives and an `if` guard -- and ends in `=>`. That is a
# line count, not a name count: a stable measure of how much name-matching
# dispatch is left, which every migration slice lowers. A new method goes in
# as a row, so neither count may go up.
#
# Counts may go DOWN, never up. When your change lowers one, re-cut with
#
#   scripts/check-method-arms.sh --update
#
# and commit the new baseline with it. `--self-test` proves the pattern still
# matches what this prose says it matches.
set -euo pipefail

cd "$(dirname "$0")/.."

BASELINE_FILE=scripts/method-arms-baseline.txt

ARM_RE='^[[:space:]]*"[A-Za-z_][A-Za-z0-9:_-]*"([[:space:]]*\|[[:space:]]*"[^"]*")*[[:space:]]*(if .*)?=>'

count_in() { # paths...
    { grep -rEh "$ARM_RE" "$@" --include='*.rs' 2>/dev/null || true; } | grep -c '' || true
}

pure_count() { count_in src/builtins/methods_0arg src/builtins/methods_narg; }
slow_count() {
    # shellcheck disable=SC2046
    count_in $(ls -d src/runtime/methods*.rs src/runtime/methods_*/ 2>/dev/null)
}

self_test() {
    local dir got
    dir=$(mktemp -d)
    trap 'rm -rf "$dir"' RETURN
    cat >"$dir/a.rs" <<'RS'
        "elems" => {
        "Str" | "Stringy" => Some(x),
        "numerator" if n > 0 => x,
        "IO::Path" => x,
        "write-int8" => x,
    // "elems" => in a comment still starts with `//`, not a quote
        let s = "elems";
        foo("x") => bar,
        "has space" => x,
RS
    got=$(count_in "$dir")
    [ "$got" = "5" ] || { echo "check-method-arms self-test: expected 5, got $got" >&2; return 1; }
    echo "check-method-arms: self-test ok"
}

if [ "${1:-}" = "--self-test" ]; then
    self_test
    exit 0
fi

pure=$(pure_count)
slow=$(slow_count)

if [ "${1:-}" = "--update" ]; then
    printf 'pure %s\nslow %s\n' "$pure" "$slow" >"$BASELINE_FILE"
    echo "method-arms baseline updated: pure=$pure slow=$slow"
    exit 0
fi

failed=0
check() { # $1 = label, $2 = current
    local base
    base=$(awk -v k="$1" '$1 == k { print $2 }' "$BASELINE_FILE")
    if [ -z "$base" ]; then
        echo "check-method-arms: no baseline entry for '$1'" >&2
        failed=1
        return
    fi
    if [ "$2" -gt "$base" ]; then
        echo "check-method-arms: $1 rose from $base to $2" >&2
        failed=1
    elif [ "$2" -lt "$base" ]; then
        echo "check-method-arms: $1 fell from $base to $2 -- re-cut the baseline:" >&2
        echo "  scripts/check-method-arms.sh --update" >&2
        failed=1
    else
        echo "check-method-arms: $1 $2 (baseline $base)"
    fi
}

check pure "$pure"
check slow "$slow"

if [ "$failed" -ne 0 ]; then
    cat >&2 <<'MSG'

  A built-in method is a row in src/builtins/method_table/ (ADR-11276), not a
  new `"name" => ...` arm in a dispatch cascade. Add the handler function and
  its `MethodRow` in the family module for the type Rakudo declares the
  method on; a cascade arm that still has to serve other receivers calls the
  same handler, so the method keeps one implementation.

  See docs/adr/11276-built-in-methods-are-handler-rows.md.
MSG
    exit 1
fi
