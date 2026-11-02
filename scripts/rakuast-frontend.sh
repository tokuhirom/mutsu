#!/bin/bash
# The RakuAST write-direction ratchet (ADR-10723; RakuAST C1, #15759).
#
# Since Stage 3 the default frontend runs every compilation unit as `parse ->
# RakuAST -> lower` (src/rakuast/frontend.rs), so "passes by default" is what
# `make test` already gates. One ratchet remains here:
#
# - `eval-check` runs ci/rakuast-ast-eval-passing.txt through
#   scripts/run-t-ast-eval.sh: each file runs as `EVAL(slurp($file).AST)`, the
#   write direction of the RakuAST layer through the public API. It replaced the
#   `MUTSU_RAKUAST=convert` test mode (and its ci/rakuast-frontend-passing.txt),
#   which is gone. The read direction is `scripts/ast-text-corpus.sh check`.
#
# Usage:
#   scripts/rakuast-frontend.sh eval-check   # run the listed files; fail if any fails
#   scripts/rakuast-frontend.sh eval-update  # run every t/ file as EVAL($ast); add the new passes to the list
#
# A file that reads `$?FILE`, `$?DIR`, `$*PROGRAM` or `$?LINE` names the driver
# (scripts/ast-eval-driver.raku) when run this way, as it would on rakudo, so it
# can never join the list.
#
# Run it against a release build with the precompile cache removed
# (`rm -rf ~/.cache/mutsu`): CI is always cold, and a warm cache hid failures.
#
#   MUTSU_BIN  interpreter to run (default target/release/mutsu)
#   JOBS       parallel jobs (default: nproc)

set -euo pipefail
cd "$(dirname "$0")/.."

MUTSU_BIN="${MUTSU_BIN:-target/release/mutsu}"
JOBS="${JOBS:-$(nproc)}"
EVAL_LIST=ci/rakuast-ast-eval-passing.txt
export MUTSU_BIN

eval_listed_files() {
  grep -v -e '^#' -e '^$' "$EVAL_LIST" || true
}

case "${1:-eval-check}" in
  eval-check)
    missing=0
    while read -r f; do
      if [ ! -f "$f" ]; then
        echo "rakuast-frontend: $f is listed in $EVAL_LIST but does not exist; remove it" >&2
        missing=1
      fi
    done < <(eval_listed_files)
    [ "$missing" -eq 0 ] || exit 1
    # prove names each failing file, which a bare count would not.
    mapfile -t files < <(eval_listed_files)
    echo "rakuast-frontend: ${#files[@]} files must pass as EVAL(slurp(\$file).AST)"
    prove -j"$JOBS" --merge -e scripts/run-t-ast-eval.sh "${files[@]}"
    ;;
  eval-update)
    mapfile -t all < <(find t -name '*.t' | LC_ALL=C sort)
    pass=$(printf '%s\n' "${all[@]}" | xargs -P "$JOBS" -I{} sh -c \
      'if scripts/run-t-ast-eval.sh "$1" >/dev/null 2>&1; then echo "$1"; fi' _ {} | LC_ALL=C sort)
    echo "rakuast-frontend: $(printf '%s\n' "$pass" | grep -c . || true) / ${#all[@]} t/ files pass as EVAL(slurp(\$file).AST)"
    # A ratchet only grows: keep every file already listed that still exists.
    {
      sed -n '/^#/p' "$EVAL_LIST"
      { eval_listed_files | while read -r f; do [ -f "$f" ] && echo "$f"; done; printf '%s\n' "$pass"; } |
        grep . | LC_ALL=C sort -u
    } > "$EVAL_LIST.new"
    mv "$EVAL_LIST.new" "$EVAL_LIST"
    echo "rakuast-frontend: $EVAL_LIST now lists $(eval_listed_files | wc -l) files"
    echo "rakuast-frontend: run 'scripts/rakuast-frontend.sh eval-check' (cold cache) before committing the list" >&2
    ;;
  *)
    echo "usage: $0 eval-check|eval-update" >&2
    exit 2
    ;;
esac
