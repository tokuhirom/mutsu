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
# - SECTION TIMING. A benchmark may print one stdout line
#       bench-section-seconds: <seconds>
#   timing the operation it exists to measure, from inside the process. Its
#   whole-script row is still recorded as usual, and an extra `<name>@section`
#   row (and `<name>@section+jit`) records the median of those in-process
#   times, with raku's own in-process median in the raku column. Use it when
#   the operation is small next to either interpreter's startup and module
#   loading, where the whole-script ratio is dominated by startup and reads
#   near 1 regardless of how slow the operation is (#8673:
#   benchmarks/bench-json-fast-spdx.raku). The section row's min column is the
#   in-process minimum.
set -euo pipefail

MUTSU=${MUTSU_BIN:-target/release/mutsu}
RAKU=${RAKU_BIN:-raku}
RUNS=${BENCH_RUNS:-7}
TIMEOUT=${BENCH_TIMEOUT:-120}

OUT_FILE=$(mktemp)
trap 'rm -f "$OUT_FILE"' EXIT

# cmd... -> "wall section" (4 decimals each; section is NA when the benchmark
# printed no `bench-section-seconds:` line), non-zero on failure.
measure_once() {
    local start end section
    start=$(date +%s%N)
    timeout "$TIMEOUT" "$@" >"$OUT_FILE" 2>/dev/null || return 1
    end=$(date +%s%N)
    section=$(awk '/^bench-section-seconds: [0-9.]+$/ {v=$2} END {print (v=="" ? "NA" : v)}' "$OUT_FILE")
    awk -v s="$start" -v e="$end" -v sec="$section" \
        'BEGIN{ printf "%.4f %s\n", (e-s)/1e9, (sec=="NA" ? "NA" : sprintf("%.4f", sec)) }'
}

stats() { # stdin: one seconds value per line -> "median min"
    sort -n | awk '{a[NR]=$1} END {
        if (NR==0) { print "NA NA"; exit }
        m = (NR%2) ? a[(NR+1)/2] : (a[NR/2]+a[NR/2+1])/2
        printf "%.4f %.4f\n", m, a[1]
    }'
}

# $1=binary $2=bench-file -> "median min section_median section_min"
# (each NA when unmeasured; the section pair is NA unless every timed run
# printed a section line).
bench_binary() {
    local bin=$1 bench=$2 i t wall sec times="" secs="" sec_ok=1
    # Warmup run (page cache, precomp); its time is discarded.
    measure_once "$bin" "$bench" >/dev/null || { echo "NA NA NA NA"; return; }
    for i in $(seq "$RUNS"); do
        t=$(measure_once "$bin" "$bench") || { echo "NA NA NA NA"; return; }
        read -r wall sec <<<"$t"
        times+="$wall"$'\n'
        if [ "$sec" = NA ]; then sec_ok=0; else secs+="$sec"$'\n'; fi
    done
    if [ "$sec_ok" = 1 ]; then
        echo "$(printf '%s' "$times" | stats) $(printf '%s' "$secs" | stats)"
    else
        echo "$(printf '%s' "$times" | stats) NA NA"
    fi
}

ratio() { # mutsu raku -> "m/r" (2 decimals) or NA
    if [ "$1" != NA ] && [ "$2" != NA ]; then
        awk -v m="$1" -v r="$2" 'BEGIN{ if (r > 0) printf "%.2f", m / r; else print "NA" }'
    else
        echo NA
    fi
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
    read -r m_med m_min ms_med ms_min <<<"$(export MUTSU_JIT=off; bench_binary "$MUTSU" "$bench")"
    if [ "$have_raku" = 1 ]; then
        read -r r_med _ rs_med _ <<<"$(bench_binary "$RAKU" "$bench")"
    else
        r_med=NA
        rs_med=NA
    fi
    printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$name" "$m_med" "$m_min" "$r_med" "$(ratio "$m_med" "$r_med")" "$RUNS"

    # JIT-on pass (ADR-0004 layer 4; the default configuration since J5):
    # recorded as its own benchmark name so the history/regression tooling
    # treats it as a separate series. The raku median from the plain pass
    # above is reused for the ratio (same runner, same job — measuring raku
    # twice would only add noise).
    read -r j_med j_min js_med js_min <<<"$(export MUTSU_JIT=on; bench_binary "$MUTSU" "$bench")"
    printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$name+jit" "$j_med" "$j_min" "$r_med" "$(ratio "$j_med" "$r_med")" "$RUNS"

    # Section series (see the header): only for a benchmark that reports one.
    # Emitted after both whole-script rows so a consumer reading rows by name
    # sees nothing new for any other benchmark.
    if [ "$ms_med" != NA ] || [ "$js_med" != NA ]; then
        printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$name@section" "$ms_med" "$ms_min" "$rs_med" "$(ratio "$ms_med" "$rs_med")" "$RUNS"
        printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$name@section+jit" "$js_med" "$js_min" "$rs_med" "$(ratio "$js_med" "$rs_med")" "$RUNS"
    fi
done
