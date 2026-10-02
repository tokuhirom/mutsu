#!/bin/bash
# The RakuAST round-trip frontend ratchet (ADR-10723 Stage 0, #10733).
#
# Under MUTSU_RAKUAST=1 every compilation unit runs as `parse -> RakuAST ->
# lower` (src/rakuast/frontend.rs). ci/rakuast-frontend-passing.txt lists the
# t/ files that pass in that mode; the list may only grow.
#
# Usage:
#   scripts/rakuast-frontend.sh check    # run the listed files in the mode; fail if any fails
#   scripts/rakuast-frontend.sh survey   # run every t/ file in the mode; print the count
#   scripts/rakuast-frontend.sh update   # survey, then rewrite the list (only ever adds files)
#
#   MUTSU_BIN  interpreter to run (default target/release/mutsu)
#   JOBS       parallel jobs (default: nproc)

set -euo pipefail
cd "$(dirname "$0")/.."

MUTSU_BIN="${MUTSU_BIN:-target/release/mutsu}"
JOBS="${JOBS:-$(nproc)}"
LIST=ci/rakuast-frontend-passing.txt
export MUTSU_BIN MUTSU_RAKUAST="${MUTSU_RAKUAST:-1}"

# Prints the files among "$@" that pass in the mode.
passing_files() {
  printf '%s\n' "$@" | xargs -P "$JOBS" -I{} sh -c \
    'if scripts/run-t-test.sh "$1" >/dev/null 2>&1; then echo "$1"; fi' _ {} | LC_ALL=C sort
}

listed_files() {
  grep -v -e '^#' -e '^$' "$LIST" || true
}

case "${1:-check}" in
  check)
    missing=0
    while read -r f; do
      if [ ! -f "$f" ]; then
        echo "rakuast-frontend: $f is listed in $LIST but does not exist; remove it" >&2
        missing=1
      fi
    done < <(listed_files)
    [ "$missing" -eq 0 ] || exit 1
    # prove names each failing file, which a bare count would not.
    mapfile -t files < <(listed_files)
    echo "rakuast-frontend: ${#files[@]} files must pass under MUTSU_RAKUAST=1"
    prove -j"$JOBS" --merge -e scripts/run-t-test.sh "${files[@]}"
    ;;
  survey | update)
    mapfile -t all < <(find t -name '*.t' | LC_ALL=C sort)
    pass=$(passing_files "${all[@]}")
    count=$(printf '%s\n' "$pass" | grep -c . || true)
    echo "rakuast-frontend: $count / ${#all[@]} t/ files pass under MUTSU_RAKUAST=1"
    if [ "$1" = update ]; then
      # A ratchet only grows: keep every file already listed that still exists,
      # even if it failed this survey (`check` is what reports that).
      {
        sed -n '/^#/p' "$LIST"
        { listed_files | while read -r f; do [ -f "$f" ] && echo "$f"; done; printf '%s\n' "$pass"; } |
          grep . | LC_ALL=C sort -u
      } > "$LIST.new"
      mv "$LIST.new" "$LIST"
      echo "rakuast-frontend: $LIST now lists $(listed_files | wc -l) files"
    fi
    ;;
  *)
    echo "usage: $0 check|survey|update" >&2
    exit 2
    ;;
esac
