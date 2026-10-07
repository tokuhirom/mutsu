#!/bin/bash
# The `.AST` text corpus: how close mutsu's `.AST` text is to rakudo's
# (RakuAST roadmap #7564, slice S9).
#
# Every Nth `t/**/*.t` is parsed with `slurp($file).AST` by rakudo and by mutsu;
# the `.raku` text of each top-level statement is compared. The share of
# identical statements, and the differences by class, are the measure.
#
# Usage:
#   scripts/ast-text-corpus.sh sample [N]         # every Nth t/ file (default 8) -> tmp/ast-text/sample.txt
#   scripts/ast-text-corpus.sh run rakudo|mutsu   # the statements' text -> tmp/ast-text/<which>/
#   scripts/ast-text-corpus.sh compare            # identical share and differing classes
#   scripts/ast-text-corpus.sh show CLASS [N]     # N example hunks of one class
#
#   MUTSU_BIN  interpreter to run (default target/debug/mutsu)
#   JOBS       parallel jobs (default: nproc)
#
# `run rakudo` is cached per file; delete tmp/ast-text/rakudo to refresh it. A
# file mutsu refuses to convert leaves an empty output (its message is in
# `<file>.err`) and is left out of the comparison.

set -euo pipefail
cd "$(dirname "$0")/.."

OUT=tmp/ast-text
SAMPLE="$OUT/sample.txt"
MUTSU_BIN="${MUTSU_BIN:-target/debug/mutsu}"
JOBS="${JOBS:-$(nproc)}"
TOOLS=scripts/ast-text-corpus

run_one() {
  local which="$1" file="$2"
  local name dir
  name=$(echo "$file" | tr '/' '_')
  dir="$OUT/$which"
  mkdir -p "$dir"
  if [ "$which" = rakudo ]; then
    [ -e "$dir/$name.out" ] && return 0
    if timeout 60 raku "$TOOLS/dump.raku" "$file" > "$dir/$name.tmp" 2> "$dir/$name.err"; then
      mv "$dir/$name.tmp" "$dir/$name.out"
    else
      rm -f "$dir/$name.tmp"
    fi
  else
    timeout 60 "$MUTSU_BIN" "$TOOLS/dump.raku" "$file" > "$dir/$name.out" 2> "$dir/$name.err" || true
  fi
}

case "${1:-}" in
  sample)
    mkdir -p "$OUT"
    find t -name '*.t' | LC_ALL=C sort | awk -v n="${2:-8}" 'NR % n == 0' > "$SAMPLE"
    wc -l < "$SAMPLE"
    ;;
  run)
    which="${2:?rakudo or mutsu}"
    [ -e "$SAMPLE" ] || "$0" sample
    export -f run_one
    export OUT TOOLS MUTSU_BIN
    xargs -P "$JOBS" -I{} bash -c 'run_one "$0" "$1"' "$which" {} < "$SAMPLE"
    ;;
  compare)
    python3 "$TOOLS/compare.py" "$OUT/rakudo" "$OUT/mutsu"
    ;;
  show)
    python3 "$TOOLS/compare.py" "$OUT/rakudo" "$OUT/mutsu" --show "${2:?class}" "${3:-6}"
    ;;
  *)
    sed -n '2,22p' "$0"
    exit 2
    ;;
esac
