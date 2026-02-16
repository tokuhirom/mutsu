#!/bin/bash
# Compare parse output between raku, mutsu --parser=rd, and mutsu --parser=nom.
#
# Usage:
#   ./scripts/compare-ast.sh 'say 42'
#   ./scripts/compare-ast.sh roast/S02-literals/numeric.t

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <code-or-file>" >&2
    exit 1
fi

INPUT="$1"
MUTSU="./target/debug/mutsu"

if [ ! -x "$MUTSU" ]; then
    echo "Building mutsu..." >&2
    cargo build 2>/dev/null
fi

TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

# Determine if input is a file or inline code
if [ -f "$INPUT" ]; then
    RAKU_ARGS=("--target=parse" "$INPUT")
    MUTSU_ARGS_NEW=("--dump-ast" "--parser=nom" "$INPUT")
    echo "=== Input: $INPUT ==="
else
    RAKU_ARGS=("--target=parse" "-e" "$INPUT")
    MUTSU_ARGS_NEW=("--dump-ast" "--parser=nom" "-e" "$INPUT")
    echo "=== Input: '$INPUT' ==="
fi

echo ""

# 1. raku --target=parse
echo "--- raku --target=parse ---"
if raku "${RAKU_ARGS[@]}" > "$TMPDIR/raku.txt" 2>&1; then
    cat "$TMPDIR/raku.txt"
else
    echo "(raku parse failed)"
    cat "$TMPDIR/raku.txt"
fi

echo ""

# 2. mutsu --parser=nom --dump-ast
echo "--- mutsu --parser=nom --dump-ast ---"
if "$MUTSU" "${MUTSU_ARGS_NEW[@]}" > "$TMPDIR/nom.txt" 2>&1; then
    cat "$TMPDIR/nom.txt"
else
    echo "(nom parse failed)"
    cat "$TMPDIR/nom.txt"
fi

echo ""

# 3. Diff between rd and nom
echo "--- diff: rd vs nom ---"
if diff -u "$TMPDIR/raku.txt" "$TMPDIR/nom.txt" > "$TMPDIR/diff.txt" 2>&1; then
    echo "(identical)"
else
    cat "$TMPDIR/diff.txt"
fi
