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
# per-file baseline that must keep passing is batteries-whitelist.txt; files the
# gate must never run at all are batteries-exclude.txt (see that file for why).
#
# Environment:
#   MUTSU_BIN             mutsu binary to run (default: target/release/mutsu)
#   BATTERIES_LOCK        manifest path (default: batteries.lock)
#   BATTERIES_WHITELIST   baseline path (default: batteries-whitelist.txt)
#   BATTERIES_EXCLUDE     exclusion list (default: batteries-exclude.txt)
#
# The path overrides exist so the gate itself can be exercised against a
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
EXCLUDE="${BATTERIES_EXCLUDE:-batteries-exclude.txt}"
WORK="$ROOT/tmp/battery-testsuite"

MODE="gate"
[ "${1:-}" = "--update" ] && MODE="update"

if [ ! -x "$MUTSU_BIN" ]; then
  echo "error: mutsu binary not found/executable at $MUTSU_BIN" >&2
  echo "  build it first (cargo build --release) or set MUTSU_BIN" >&2
  exit 2
fi
# Tests run with their own repo as the working directory (below), so the binary
# must be addressable from there.
case "$MUTSU_BIN" in /*) ;; *) MUTSU_BIN="$ROOT/$MUTSU_BIN" ;; esac

# DBIish's own CommonTesting harness no-ops its write-path assertions (a bare
# `skip-rest` covering the whole planned count) unless this is set — without
# it, every DBIish file that reaches a live-write test trivially "passes" by
# skipping, giving zero regression coverage. Harmless for every other bundled
# suite, which does not read this variable.
export DBIISH_WRITE_TEST=YES

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
#
# $1 is the working directory to run in — the fetched repo root. These suites are
# written to be run from their own checkout (`prove` / `zef test` do exactly
# that) and reach for fixtures by RELATIVE path, e.g. OpenSSL's 03-rsa does
# `slurp 't/key.pem'`. Running from the mutsu repo root instead made such files
# die before their first test and be miscounted as library failures.
# A `not ok N - desc # TODO reason` line is an *expected* failure: TAP says the
# suite still passes, and `prove` agrees. Upstream suites use it for assertions
# that depend on the host (NativeLibs' 10-search only finds a versioned
# `libmysqlclient` on some distros), so counting it as a failure would make a file
# ungateable even at exact parity with raku — which fails the same subtest.
run_one() {
  local workdir="$1"; shift
  local out planned nok okc todo
  out="$(cd "$workdir" && timeout 120 "$MUTSU_BIN" "$@" 2>&1)"
  planned="$(printf '%s\n' "$out" | grep -oE '^1\.\.[0-9]+' | head -1 | cut -d. -f3)"
  nok="$(printf '%s\n' "$out" | grep -cE '^not ok')"
  okc="$(printf '%s\n' "$out" | grep -cE '^ok ')"
  todo="$(printf '%s\n' "$out" | grep -ciE '^not ok.*# *TODO')"
  if [ -n "$planned" ] \
     && [ "$((nok - todo))" -eq 0 ] \
     && [ "$((okc + todo))" -eq "$planned" ]; then
    if [ "$todo" -gt 0 ]; then
      echo "PASS($todo todo)"
    else
      echo "PASS"
    fi
    return 0
  fi
  echo "FAIL(ok=$okc/${planned:-?},notok=$nok,todo=$todo)"
  return 1
}

# Read a tab-separated lock, skipping comments/blank lines and the header row.
lock_rows() {
  grep -vE '^[[:space:]]*#|^[[:space:]]*$' "$LOCK" | grep -vE '^name[[:space:]]'
}

# Is this test file on the do-not-run list? (`name<TAB>file`, comments allowed.)
is_excluded() {
  [ -f "$EXCLUDE" ] || return 1
  grep -vE '^[[:space:]]*#|^[[:space:]]*$' "$EXCLUDE" \
    | grep -qxF "$(printf '%s\t%s' "$1" "$2")"
}

rm -rf "$WORK"
mkdir -p "$WORK"

# Accumulate the freshly-observed pass set (for --update) and the gate verdict.
NEW_WHITELIST="$(mktemp)"
REGRESSED=0
SETUP_FAILED=0
TOTAL_PASS=0
TOTAL_FILES=0
TOTAL_EXCLUDED=0

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
    # Address the test relative to its own repo — run_one runs it from there.
    rel="${f#"$clone"/}"
    # Excluded files are not run at all, in either mode: their verdict depends
    # on something outside this repository (see batteries-exclude.txt), so they
    # can neither block a release nor enter the baseline.
    if is_excluded "$name" "$base"; then
      printf '  %-40s %s\n' "$base" "SKIP(excluded)"
      TOTAL_EXCLUDED=$((TOTAL_EXCLUDED + 1))
      continue
    fi
    TOTAL_FILES=$((TOTAL_FILES + 1))
    verdict="$(run_one "$clone" "${inc[@]}" "$rel")"
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
echo "=== summary: $TOTAL_PASS/$TOTAL_FILES test files pass ($TOTAL_EXCLUDED excluded) ==="

if [ "$MODE" = "update" ]; then
  cp "$NEW_WHITELIST" "$WHITELIST"
  echo "wrote $WHITELIST ($(wc -l < "$WHITELIST") files)"
  rm -f "$NEW_WHITELIST"
  [ "$SETUP_FAILED" -eq 0 ]
  exit $?
fi

# Gate mode: also flag any whitelisted file that never ran (e.g. removed
# upstream while still whitelisted) as a regression. An *excluded* file is a
# deliberate non-run, not a regression — it should have been dropped from the
# whitelist by `--update`, but say so plainly rather than failing the release.
if [ -f "$WHITELIST" ]; then
  while IFS= read -r line; do
    [ -n "$line" ] || continue
    if ! grep -qxF "$line" "$NEW_WHITELIST"; then
      if grep -vE '^[[:space:]]*#|^[[:space:]]*$' "$EXCLUDE" 2>/dev/null \
         | grep -qxF "$line"; then
        echo "  note: whitelisted '$line' is also excluded — re-run --update" >&2
        continue
      fi
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
