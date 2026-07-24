#!/usr/bin/env bash
#
# Release-time gate: run every bundled library's upstream test suite against the
# BUNDLED library + the built `mutsu`, and fail if any whitelisted test file has
# regressed. See BATTERIES.md and docs/batteries/testsuite-gate.md.
#
#   scripts/battery-testsuite.sh            # gate mode: enforce the whitelist
#   scripts/battery-testsuite.sh --update   # regenerate batteries-whitelist.txt
#
# The set of batteries and where their tests come from is batteries.lock; the
# per-file baseline that must keep passing is batteries-whitelist.txt.
#
# Environment:
#   MUTSU_BIN             mutsu binary to run (default: target/release/mutsu)
#   BATTERIES_LOCK        manifest path (default: batteries.lock)
#   BATTERIES_WHITELIST   baseline path (default: batteries-whitelist.txt)
#
# The two path overrides exist so the gate itself can be exercised against a
# scratch manifest/baseline (e.g. to verify that a regression really does fail)
# without disturbing the committed files.
#
# Exit status: 0 if every whitelisted file passes (gate mode) or the whitelist
# was rewritten (update mode); non-zero if a whitelisted file regressed or setup
# failed.
set -u

cd "$(dirname "$0")/.."
ROOT="$(pwd)"
MUTSU_BIN="${MUTSU_BIN:-target/release/mutsu}"
LOCK="${BATTERIES_LOCK:-batteries.lock}"
WHITELIST="${BATTERIES_WHITELIST:-batteries-whitelist.txt}"
WORK="tmp/battery-testsuite"

MODE="gate"
[ "${1:-}" = "--update" ] && MODE="update"

if [ ! -x "$MUTSU_BIN" ]; then
  echo "error: mutsu binary not found/executable at $MUTSU_BIN" >&2
  echo "  build it first (cargo build --release) or set MUTSU_BIN" >&2
  exit 2
fi

# --- fetch a specific upstream commit into $dir (shallow, no full history) ----
fetch_commit() {
  local dir="$1" url="$2" commit="$3"
  rm -rf "$dir"
  mkdir -p "$dir"
  git -C "$dir" init -q
  git -C "$dir" remote add origin "$url"
  # GitHub allows fetching an arbitrary reachable sha directly.
  if ! git -C "$dir" fetch -q --depth 1 origin "$commit" 2>/dev/null; then
    echo "error: could not fetch $commit from $url" >&2
    return 1
  fi
  git -C "$dir" checkout -q FETCH_HEAD
}

# --- run one test file; echo PASS or FAIL(detail); return 0 iff it fully passes
run_one() {
  local out planned nok okc
  out="$(timeout 120 "$MUTSU_BIN" "$@" 2>&1)"
  planned="$(printf '%s\n' "$out" | grep -oE '^1\.\.[0-9]+' | head -1 | cut -d. -f3)"
  nok="$(printf '%s\n' "$out" | grep -cE '^not ok')"
  okc="$(printf '%s\n' "$out" | grep -cE '^ok ')"
  if [ -n "$planned" ] && [ "$nok" -eq 0 ] && [ "$okc" -eq "$planned" ]; then
    echo "PASS"
    return 0
  fi
  echo "FAIL(ok=$okc/${planned:-?},notok=$nok)"
  return 1
}

# Read a tab-separated lock, skipping comments/blank lines and the header row.
lock_rows() {
  grep -vE '^[[:space:]]*#|^[[:space:]]*$' "$LOCK" | grep -vE '^name[[:space:]]'
}

rm -rf "$WORK"
mkdir -p "$WORK"

# Accumulate the freshly-observed pass set (for --update) and the gate verdict.
NEW_WHITELIST="$(mktemp)"
REGRESSED=0
SETUP_FAILED=0
TOTAL_PASS=0
TOTAL_FILES=0

while IFS=$'\t' read -r name bundled_lib test_url commit test_glob extra_includes; do
  [ -n "$name" ] || continue
  echo "=== battery: $name (commit ${commit:0:12}) ==="

  clone="$WORK/$(printf '%s' "$name" | tr -c 'A-Za-z0-9._-' '_')"
  if ! fetch_commit "$clone" "$test_url" "$commit"; then
    SETUP_FAILED=1
    continue
  fi

  # Build the -I list: the bundled library first, then any extra includes
  # ({clone} expands to the fetched repo root; `-` means none).
  inc=(-I "$ROOT/$bundled_lib")
  if [ "$extra_includes" != "-" ]; then
    IFS=',' read -r -a extras <<< "$extra_includes"
    for e in "${extras[@]}"; do
      e="${e//\{clone\}/$clone}"
      case "$e" in
        /*) inc+=(-I "$e") ;;
        *)  inc+=(-I "$ROOT/$e") ;;
      esac
    done
  fi

  shopt -s nullglob
  files=("$clone"/$test_glob)
  shopt -u nullglob
  if [ "${#files[@]}" -eq 0 ]; then
    echo "  warning: no test files matched '$test_glob'" >&2
    SETUP_FAILED=1
    continue
  fi

  for f in "${files[@]}"; do
    base="$(basename "$f")"
    TOTAL_FILES=$((TOTAL_FILES + 1))
    verdict="$(run_one "${inc[@]}" "$f")"
    rc=$?
    printf '  %-40s %s\n' "$base" "$verdict"
    if [ "$rc" -eq 0 ]; then
      TOTAL_PASS=$((TOTAL_PASS + 1))
      printf '%s\t%s\n' "$name" "$base" >> "$NEW_WHITELIST"
    fi
    # Gate: a file listed in the whitelist MUST pass.
    if [ "$MODE" = "gate" ] && [ -f "$WHITELIST" ] \
       && grep -qxF "$(printf '%s\t%s' "$name" "$base")" "$WHITELIST" \
       && [ "$rc" -ne 0 ]; then
      echo "  REGRESSION: whitelisted $name/$base no longer passes" >&2
      REGRESSED=1
    fi
  done
done < <(lock_rows)

LC_ALL=C sort -o "$NEW_WHITELIST" "$NEW_WHITELIST"

echo
echo "=== summary: $TOTAL_PASS/$TOTAL_FILES test files pass ==="

if [ "$MODE" = "update" ]; then
  cp "$NEW_WHITELIST" "$WHITELIST"
  echo "wrote $WHITELIST ($(wc -l < "$WHITELIST") files)"
  rm -f "$NEW_WHITELIST"
  [ "$SETUP_FAILED" -eq 0 ]
  exit $?
fi

# Gate mode: also flag any whitelisted file that never ran (e.g. removed
# upstream while still whitelisted) as a regression.
if [ -f "$WHITELIST" ]; then
  while IFS= read -r line; do
    [ -n "$line" ] || continue
    if ! grep -qxF "$line" "$NEW_WHITELIST"; then
      echo "  REGRESSION: whitelisted '$line' did not pass this run" >&2
      REGRESSED=1
    fi
  done < "$WHITELIST"
fi
rm -f "$NEW_WHITELIST"

if [ "$SETUP_FAILED" -ne 0 ]; then
  echo "GATE ERROR: a battery test suite could not be set up (see above)." >&2
  exit 2
fi
if [ "$REGRESSED" -ne 0 ]; then
  echo "GATE FAILED: a bundled library regressed below its recorded baseline." >&2
  exit 1
fi
echo "GATE PASSED: every whitelisted bundled-library test still passes."
