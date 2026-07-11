#!/usr/bin/env bash
# Record how the reference `raku` interpreter fares on every roast test,
# as an oracle baseline for prioritising mutsu work.
#
# Output: TODO_roast/raku-baseline.tsv
#   columns: path, plan, ok, notok, todo, sorry, exit, raku_status, whitelisted
#
# IMPORTANT CAVEATS (see TODO_roast/raku-baseline.md for the full writeup):
#   * This runs `raku` DIRECTLY on the raw .t file (UNFUDGED). Roast fudge
#     directives (#?rakudo skip/todo, #?v6, ...) are NOT applied, because
#     applying them via roast/fudge writes rewritten files under roast/, which
#     is read-only in this repo. mutsu's own runs DO apply fudge (MUTSU_FUDGE=1),
#     so a "raku FAIL/SORRY" on a whitelisted file is usually a fudge/version
#     artifact, not raku being worse than mutsu.
#   * The reference raku is whatever `raku` resolves to (record its version).
#   * raku's stdin is redirected from /dev/null so stdin-reading tests
#     (e.g. $*IN.lines) cannot swallow the driver's file list.
#
# raku_status classification:
#   PASS      plan>0 && ok==plan && notok==0
#   FAIL      ran to a plan but ok<plan or notok>0
#   SORRY     compile error (===SORRY===) -- usually 6.e-only syntax on a 6.d raku
#   ABORT     started (ok>0) but ran fewer than plan with no explicit notok (mid-file die)
#   NOPLAN    no plan line and no ok/sorry
#   TIMEOUT   killed by the per-file timeout
set -u
cd "$(dirname "$0")/.." || exit 1

RAKU="${RAKU:-raku}"
TIMEOUT="${RAKU_BASELINE_TIMEOUT:-25}"
OUT=TODO_roast/raku-baseline.tsv
TMP="$OUT.partial"

printf 'path\tplan\tok\tnotok\ttodo\tsorry\texit\traku_status\twhitelisted\n' > "$TMP"

while read -r f; do
  out=$(timeout "$TIMEOUT" "$RAKU" "$f" </dev/null 2>&1)
  ec=$?
  plan=$(printf '%s\n' "$out" | grep -oE '^1\.\.[0-9]+' | head -1 | sed 's/1\.\.//')
  ok=$(printf '%s\n' "$out" | grep -cE '^ok ')
  notok=$(printf '%s\n' "$out" | grep -cE '^not ok ')
  todo=$(printf '%s\n' "$out" | grep -ciE '# TODO')
  sorry=$(printf '%s\n' "$out" | grep -c 'SORRY')
  plan=${plan:-0}

  if [ "$ec" -eq 124 ]; then st=TIMEOUT
  elif [ "$sorry" -gt 0 ]; then st=SORRY
  elif [ "$plan" -gt 0 ] && [ "$ok" -eq "$plan" ] && [ "$notok" -eq 0 ]; then st=PASS
  elif [ "$plan" -gt 0 ] && { [ "$notok" -gt 0 ] || [ "$((ok+notok))" -ge "$plan" ]; }; then st=FAIL
  elif [ "$ok" -gt 0 ]; then st=ABORT
  else st=NOPLAN
  fi

  if grep -qxF "$f" roast-whitelist.txt; then wl=1; else wl=0; fi

  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$f" "$plan" "$ok" "$notok" "$todo" "$sorry" "$ec" "$st" "$wl" >> "$TMP"
done < <(find roast -name '*.t' -type f | LC_ALL=C sort)

mv "$TMP" "$OUT"
echo "DONE: $(($(wc -l < "$OUT") - 1)) rows -> $OUT"
