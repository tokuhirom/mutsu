#!/usr/bin/env bash
# Self-test of scripts/frontend-cost.sh (run by `make check-frontend-debt`): the measurement must
# fail, not report a number, whenever a run did not finish or printed nothing usable. The script
# is not exercised by CI otherwise, so a regression here would turn a broken measurement into a
# passing check. Uses a fake valgrind and a fake binary; no real profiling build is needed.
set -euo pipefail

cd "$(dirname "$0")/.."
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT

cat >"$T/valgrind" <<'EOF'
#!/usr/bin/env bash
# FAKE_MODE: ok | exit127 | exit124 | nothing | collected-only | zero
mode=${FAKE_MODE:-ok}
toggle=0
for a in "$@"; do case $a in --toggle-collect=*) toggle=1 ;; esac; done
case $mode in
exit127) exit 127 ;;
exit124) echo "==1== Collected : 1" >&2; exit 124 ;;
nothing) exit 0 ;;
collected-only) [ $toggle = 1 ] && echo "==1== Collected : 1234" >&2; exit 0 ;;
zero) echo "==1== Collected : 0" >&2; echo "==1== I   refs:      0" >&2; exit 0 ;;
*) if [ $toggle = 1 ]; then echo "==1== Collected : 1234" >&2; else echo "==1== I   refs:      5,678" >&2; fi ;;
esac
EOF
chmod +x "$T/valgrind"
printf '#!/usr/bin/env bash\nexit "${FAKE_BIN_EXIT:-0}"\n' >"$T/mutsu"
chmod +x "$T/mutsu"

run() { # run MODE [ENV=VAL...] -> exit status of `check`
    local mode=$1
    shift
    env FAKE_MODE="$mode" "$@" FRONTEND_COST_VALGRIND="$T/valgrind" FRONTEND_COST_LOGDIR="$T/logs" \
        scripts/frontend-cost.sh measure "$T/mutsu" >"$T/out" 2>"$T/err"
}

fail=0
expect_fail() { # expect_fail NAME MODE [ENV=VAL...]
    local name=$1
    shift
    if run "$@"; then
        echo "frontend-cost self-test: '$name' must fail but succeeded" >&2
        fail=1
    fi
}

# The honest run succeeds and reports all five cases.
run ok || { echo "frontend-cost self-test: the ok run failed:" >&2; cat "$T/err" >&2; fail=1; }
[ "$(grep -c '%' "$T/out" || true)" = 5 ] || { echo "frontend-cost self-test: the ok run did not report five cases" >&2; fail=1; }

expect_fail "valgrind missing (127)" exit127
expect_fail "valgrind times out (124) after printing a figure" exit124
expect_fail "no output at all" nothing
expect_fail "only the Collected line" collected-only
expect_fail "zero instructions" zero
expect_fail "the warm run of the binary fails" ok FAKE_BIN_EXIT=3

# validate: a table must hold each case exactly once.
cases=$(sed -n 's/^    "\([^	]*\)	.*/\1/p' scripts/frontend-cost.sh)
good=$(for c in $cases; do printf '%s\t10\t20\n' "$c"; done)
printf '%s\n' "$good" | scripts/frontend-cost.sh validate 2>/dev/null \
    || { echo "frontend-cost self-test: a complete table was rejected" >&2; fail=1; }
printf '%s\n' "$good" | sed 1d | scripts/frontend-cost.sh validate 2>/dev/null \
    && { echo "frontend-cost self-test: a table with a missing case was accepted" >&2; fail=1; }
{ printf '%s\n' "$good"; printf '%s\n' "$good" | sed -n 1p; } | scripts/frontend-cost.sh validate 2>/dev/null \
    && { echo "frontend-cost self-test: a table with a duplicate case was accepted" >&2; fail=1; }
: | scripts/frontend-cost.sh validate 2>/dev/null \
    && { echo "frontend-cost self-test: an empty table was accepted" >&2; fail=1; }

[ "$fail" = 0 ] || exit 1
echo "frontend-cost self-test: ok"
