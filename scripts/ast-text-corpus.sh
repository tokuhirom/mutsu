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
# The CI ratchet (C1, #15759) uses a pinned sample of roast files instead, since CI has no `raku`:
#   scripts/ast-text-corpus.sh golden [N]         # rakudo's text of every Nth roast file, hashed
#                                                 #   -> ci/ast-text-corpus/rakudo.tsv (needs raku)
#   scripts/ast-text-corpus.sh check              # fail if a statement listed in
#                                                 #   ci/ast-text-corpus/identical.tsv stopped matching
#   scripts/ast-text-corpus.sh update             # add the statements that match now (never removes)
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

RATCHET_DIR=ci/ast-text-corpus
GOLDEN="$RATCHET_DIR/rakudo.tsv"
IDENTICAL="$RATCHET_DIR/identical.tsv"

# Run mutsu's dump over the golden's files into tmp/ast-text-ratchet/mutsu.
ratchet_run() {
  local list="$OUT/ratchet-files.txt"
  mkdir -p "$OUT"
  grep '^F' "$GOLDEN" | cut -f2 | LC_ALL=C sort -u > "$list"
  rm -rf "$OUT/ratchet-root/mutsu"
  export -f run_one
  export TOOLS MUTSU_BIN
  OUT="$OUT/ratchet-root" xargs -P "$JOBS" -I{} bash -c 'run_one "$0" "$1"' mutsu {} < "$list"
  echo "$OUT/ratchet-root/mutsu"
}

case "${1:-}" in
  golden)
    command -v raku >/dev/null || { echo "ast-text-corpus: golden needs raku" >&2; exit 1; }
    mkdir -p "$OUT" "$RATCHET_DIR"
    list="$OUT/golden-files.txt"
    find roast -name '*.t' | LC_ALL=C sort | awk -v n="${2:-12}" 'NR % n == 0' > "$list"
    export -f run_one
    export TOOLS MUTSU_BIN
    OUT="$OUT/golden-root" xargs -P "$JOBS" -I{} bash -c 'run_one "$0" "$1"' rakudo {} < "$list"
    python3 -I "$TOOLS/ratchet.py" golden "$OUT/golden-root/rakudo" "$list" "$GOLDEN"
    ;;
  check|update)
    [ -e "$GOLDEN" ] || { echo "ast-text-corpus: $GOLDEN is missing; run golden" >&2; exit 1; }
    dir=$(ratchet_run)
    python3 -I "$TOOLS/ratchet.py" "$1" "$dir" "$GOLDEN" "$IDENTICAL"
    ;;
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
