#!/usr/bin/env bash
# Deterministic benchmark metric: simulated instruction counts (issue #8085).
#
# Prints one TSV row per benchmark to stdout:
#
#   benchmark  instructions
#
# WHY THIS EXISTS, and why it is not a replacement for scripts/bench-ci.sh.
#
# The wall-clock series in bench-history.tsv has a measured noise floor of
# 16-33% (median |change| between consecutive recorded main commits, all 48
# series; p90 29-96%). Its own regression alarm fires at 1.5x, which is INSIDE
# that p90 for most rows: it rings on noise and stays silent through a genuine
# 30% regression. That is how the ~46x element-store defect of #8069 stayed
# invisible for as long as it did.
#
# `valgrind --tool=callgrind` counts executed instructions by SIMULATION, so the
# number does not depend on the runner's CPU, its load, or its neighbours --
# the entire class of variance the wall-clock series fights does not exist here.
# Measured spread over three back-to-back runs of one binary:
#
#   bench-hash          269,436,183 / 269,406,388 / 269,437,654   0.012%
#   bench-grammar-parse  50,299,105 /  50,328,170 /  50,282,570   0.09%
#   bench-fib (JIT on) 1,230,716,337 / 1,230,716,715              0.00003%
#
# So the detectable change is ~0.1% rather than 16-33%: about 200x the
# resolution, which is what makes a 2% alarm meaningful.
#
# TWO THINGS THIS NUMBER IS NOT:
#
# - It is not time. A change that removes 10% of the instructions but adds a
#   cache-missing indirection is slower, and this series will happily call it an
#   improvement. Instruction counts localize a change; the wall-clock series
#   confirms it mattered. Read them together, keep both.
# - It is not stable across toolchains. Ir is a property of the compiled
#   binary, so a rustc bump, a codegen change or a linker change steps the whole
#   series at once. That is a legible one-time step, unlike the random walk it
#   replaces -- but do not read such a step as a regression.
#
# Cost: about 4 minutes for the whole suite per configuration on a 4-core box
# (callgrind is ~90x slower than native, and the benchmarks are short). The
# JIT-on pass is cheaper than the JIT-off one, because there are fewer
# instructions to simulate.
#
# Optional: BENCH_DET_ALLOCS=1 adds an `allocations` column from a second
# `--tool=memcheck` pass. Exact and equally deterministic (identical to the byte
# across runs), but memcheck roughly triples the wall time of the whole script,
# so it is off by default and meant for local investigation rather than CI.
set -euo pipefail

MUTSU=${MUTSU_BIN:-target/release/mutsu}
# Generous: callgrind is ~90x native, and the slowest benchmark (bench-threads,
# whose workers callgrind serializes onto one core) takes ~60s here.
TIMEOUT=${BENCH_DET_TIMEOUT:-600}
WANT_ALLOCS=${BENCH_DET_ALLOCS:-0}

if ! command -v valgrind >/dev/null 2>&1; then
    echo "bench-det: valgrind not found; install it (apt-get install -y valgrind)" >&2
    exit 1
fi

instructions() { # $1=bench-file -> instruction count, or NA
    local ir
    # callgrind prints "I   refs:      269,396,968" as its last summary line;
    # that is the whole measurement, so no callgrind_annotate pass is needed
    # and the output file is discarded.
    ir=$(timeout "$TIMEOUT" valgrind --tool=callgrind --callgrind-out-file=/dev/null \
            "$MUTSU" "$1" 2>&1 >/dev/null |
         sed -n 's/.*I  *refs: *//p' | tr -d ', ') || true
    if [ -n "${ir:-}" ]; then printf '%s' "$ir"; else printf 'NA'; fi
}

allocations() { # $1=bench-file -> heap allocation count, or NA
    local n
    n=$(timeout "$TIMEOUT" valgrind --tool=memcheck --error-exitcode=0 \
            "$MUTSU" "$1" 2>&1 >/dev/null |
        sed -n 's/.*total heap usage: *\([0-9,]*\) allocs.*/\1/p' | tr -d ',') || true
    if [ -n "${n:-}" ]; then printf '%s' "$n"; else printf 'NA'; fi
}

emit() { # $1=row-name $2=bench-file
    local ir allocs
    ir=$(instructions "$2")
    if [ "$WANT_ALLOCS" = 1 ]; then
        allocs=$(allocations "$2")
        printf '%s\t%s\t%s\n' "$1" "$ir" "$allocs"
    else
        printf '%s\t%s\n' "$1" "$ir"
    fi
}

for bench in benchmarks/*.raku; do
    name=$(basename "$bench" .raku)
    # Same two series as scripts/bench-ci.sh, under the same names, so a row
    # can be read across both histories: the plain one pins MUTSU_JIT=off as
    # the interpreter baseline, `+jit` is the default configuration.
    MUTSU_JIT=off emit "$name" "$bench"
    MUTSU_JIT=on emit "$name+jit" "$bench"
done
