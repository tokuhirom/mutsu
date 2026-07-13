#!/usr/bin/env bash
# CI benchmark runner: measure every benchmarks/*.raku with 1 warmup + N timed
# runs and print one TSV row per benchmark to stdout.
#
# Columns: benchmark  mutsu_median_s  mutsu_min_s  raku_median_s  ratio  runs
#
# Notes for consumers (.github/workflows/bench.yml):
# - Shared CI runners are noisy; single absolute numbers are not comparable
#   across runs. The workflow therefore records BOTH the median-of-N absolute
#   time and the mutsu/raku ratio (raku measured on the same runner in the
#   same job), which normalizes runner-speed differences. Trends across many
#   commits are the signal; a single-run delta is not.
# - A missing/failed measurement is recorded as NA rather than failing the
#   job (bench history must not gate main).
set -euo pipefail

MUTSU=${MUTSU_BIN:-target/release/mutsu}
RAKU=${RAKU_BIN:-raku}
RUNS=${BENCH_RUNS:-7}
TIMEOUT=${BENCH_TIMEOUT:-120}

measure_once() { # cmd... -> wall seconds (4 decimals), non-zero on failure
    local start end
    start=$(date +%s%N)
    timeout "$TIMEOUT" "$@" >/dev/null 2>&1 || return 1
    end=$(date +%s%N)
    awk -v s="$start" -v e="$end" 'BEGIN{printf "%.4f\n", (e-s)/1e9}'
}

stats() { # stdin: one seconds value per line -> "median min"
    sort -n | awk '{a[NR]=$1} END {
        if (NR==0) { print "NA NA"; exit }
        m = (NR%2) ? a[(NR+1)/2] : (a[NR/2]+a[NR/2+1])/2
        printf "%.4f %.4f\n", m, a[1]
    }'
}

bench_binary() { # $1=binary $2=bench-file -> "median min" (or "NA NA")
    local bin=$1 bench=$2 i t times=""
    # Warmup run (page cache, precomp); its time is discarded.
    measure_once "$bin" "$bench" >/dev/null || { echo "NA NA"; return; }
    for i in $(seq "$RUNS"); do
        t=$(measure_once "$bin" "$bench") || { echo "NA NA"; return; }
        times+="$t"$'\n'
    done
    printf '%s' "$times" | stats
}

have_raku=0
if command -v "$RAKU" >/dev/null 2>&1; then
    have_raku=1
fi

for bench in benchmarks/*.raku; do
    name=$(basename "$bench" .raku)
    # Interpreter-only pass: MUTSU_JIT defaults to on since J5 (ADR-0004), so
    # the plain series pins JIT off explicitly to keep its historical meaning
    # (pure interpreter baseline) — the `+jit` series below is the default
    # configuration users actually run.
    read -r m_med m_min <<<"$(export MUTSU_JIT=off; bench_binary "$MUTSU" "$bench")"
    if [ "$have_raku" = 1 ]; then
        read -r r_med _ <<<"$(bench_binary "$RAKU" "$bench")"
    else
        r_med=NA
    fi
    if [ "$m_med" != NA ] && [ "$r_med" != NA ]; then
        ratio=$(awk -v m="$m_med" -v r="$r_med" \
            'BEGIN{ if (r > 0) printf "%.2f", m / r; else print "NA" }')
    else
        ratio=NA
    fi
    printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$name" "$m_med" "$m_min" "$r_med" "$ratio" "$RUNS"

    # JIT-on pass (ADR-0004 layer 4; the default configuration since J5):
    # recorded as its own benchmark name so the history/regression tooling
    # treats it as a separate series. The raku median from the plain pass
    # above is reused for the ratio (same runner, same job — measuring raku
    # twice would only add noise).
    read -r j_med j_min <<<"$(export MUTSU_JIT=on; bench_binary "$MUTSU" "$bench")"
    if [ "$j_med" != NA ] && [ "$r_med" != NA ]; then
        j_ratio=$(awk -v m="$j_med" -v r="$r_med" \
            'BEGIN{ if (r > 0) printf "%.2f", m / r; else print "NA" }')
    else
        j_ratio=NA
    fi
    printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$name+jit" "$j_med" "$j_min" "$r_med" "$j_ratio" "$RUNS"
done
