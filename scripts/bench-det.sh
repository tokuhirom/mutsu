#!/usr/bin/env bash
# Deterministic benchmark metrics: simulated instruction counts and heap
# allocation counts (issues #8085, #8959).
#
# Prints one TSV row per benchmark to stdout:
#
#   benchmark  instructions  allocations
#
# Usage:
#
#   scripts/bench-det.sh                        # the whole benchmarks/ suite (CI)
#   scripts/bench-det.sh benchmarks/bench-hash.raku [more.raku ...]
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
# THE ALLOCATION COLUMN COMES OUT OF THE SAME PASS, FOR FREE.
#
# Callgrind records a call count for every (caller, callee) edge, so the number
# of heap allocations is already in the profile it writes: it is the call count
# of the allocator entry points. Reading it costs one awk pass over the output
# file that used to be thrown away (`--callgrind-out-file=/dev/null`), not a
# second instrumented run -- which is why this column is on by default where the
# older `--tool=memcheck` pass (roughly tripling the script's wall time) was
# opt-in. The instruction counts are unaffected: nothing about the callgrind
# invocation changed except where its output goes, so the Ir series continues
# across this change.
#
# It is at least as reproducible as the instruction count. Measured over repeated
# runs of one binary:
#
#   bench-hash          237,793 / 237,793 / 237,793                 0%
#   bench-grammar-parse  38,862 /  38,858 /  38,861                 0.010%
#
# (Ir over the same three runs: 0.012% and 0.016%.) A wider two-run sample put
# eight of ten series within 0.01% and four of them at exactly zero. Where a
# count does move it is the same per-process `HashMap` seeding that moves Ir,
# reaching the allocator through a string or table whose growth depends on
# iteration order. It is also the more stable of the two across toolchains: a
# rustc bump re-codegens every instruction, but it does not change how many times
# the program asks for memory.
#
# THE ONE BIG EXCEPTION IS THE COLD-PRECOMPILATION RUN, and this column makes it
# unmissable: the tenth series in that sample was `bench-json-fast`, at 1,777,030
# allocations then 1,311,894 -- **-26%**, from nothing but the module
# precompilation cache, which the first run after a build has to populate. That
# is the same trap the perf-tuning skill's §0 documents as 650M Ir (34% of the
# run), measured on the same benchmark. A LOCAL A/B therefore has to compare warm
# against warm, or it will read a 26% allocation "win" that is only a populated
# cache.
#
# The recorded CI series measures the WARM state, and by construction rather than
# by luck: bench.yml runs the wall-clock pass (scripts/bench-ci.sh, seven runs of
# each benchmark in each lane) BEFORE this script, so the cache is thoroughly
# populated by the time callgrind starts. The first recorded rows prove it --
# `bench-json-fast` came back as 1,311,788, which is the warm local number to
# within 0.008%, not the cold one. (An earlier version of this comment claimed the
# opposite, that CI is "cold every time, consistently" because it builds fresh.
# Consistent it is, but warm; and warm is the state worth recording, since it is
# the one a steady-state run is in. If those two steps are ever reordered or the
# wall-clock pass is dropped, this series steps once, for this reason.)
#
# WHY THE SYMBOL SET BELOW, and why counting two layers would be wrong: the libc
# entry points are counted, not Rust's `__rust_alloc*` shims. The shims are
# ordinary functions that LLVM may inline into their callers -- which would
# undercount silently, and differently per codegen unit -- while `malloc` and its
# siblings are dynamic symbols valgrind always resolves. Rust's allocator lowers
# onto them one-to-one, which the profile shows directly: on `bench-hash`,
# `__rustc::__rust_alloc` 236,405 -> `__rdl_alloc` 236,405 -> `malloc` 236,419
# (the extra 14 are libc's own), plus 1,321 through `__rust_realloc` and 11
# through `__rust_alloc_zeroed`.
#
# Entry points must not be mixed with ones they themselves call: glibc's
# `reallocarray` calls `realloc`, so counting both bills one allocation twice.
# `malloc`/`calloc`/`realloc` and the memalign family (Rust uses
# `posix_memalign` for an alignment above 16) all reach `_int_malloc` /
# `_mid_memalign` independently, so counting those is not double counting --
# and counting the internals instead would be, since they are also reached from
# each other.
#
# The count is therefore every allocation the process makes, mutsu's and its C
# dependencies' alike, which is what memcheck's "total heap usage: N allocs"
# reports as well. The two agree closely but not exactly -- 237,793 here against
# memcheck's 237,590 on `bench-hash`, 0.09% -- because they are different
# instrumented runs of the program, and memcheck's own malloc replacement sees a
# slightly different set (the dynamic loader's `__minimal_malloc`, for one, is
# its own bump allocator and is in neither number). Neither is "the true count"
# to be reconciled against the other; what matters is that each is stable enough
# to compare a commit against its predecessor, which the spread above shows.
#
# What this column is NOT is bytes: callgrind counts calls, not arguments, so
# a run that allocates the same number of larger blocks looks identical.
# BENCH_DET_BYTES=1 adds a `bytes` column from a `--tool=memcheck` pass for
# local investigation; it roughly triples the script's wall time, so CI does not
# set it, and the column is appended last so a reader of the default three can
# ignore it.
#
# Cost: about 4 minutes for the whole suite per configuration on a 4-core box
# (callgrind is ~90x slower than native, and the benchmarks are short). The
# JIT-on pass is cheaper than the JIT-off one, because there are fewer
# instructions to simulate.
set -euo pipefail

MUTSU=${MUTSU_BIN:-target/release/mutsu}
# Generous: callgrind is ~90x native, and the slowest benchmark (bench-threads,
# whose workers callgrind serializes onto one core) takes ~60s here.
TIMEOUT=${BENCH_DET_TIMEOUT:-600}
WANT_BYTES=${BENCH_DET_BYTES:-0}
CG_OUT=${BENCH_DET_CG_OUT:-tmp/bench-det-cg.out}

# Sum the call counts of the allocator entry points in a callgrind output file.
# A call is recorded as a `cfn=` (called function) line followed by
# `calls=<n> <target line>`, so the count wanted is the sum of `calls=` over the
# edges whose callee is an allocator.
#
# Two properties of the format have to be honoured, and getting either wrong
# undercounts silently rather than failing:
#
#   1. Name compression. A function name is spelled out once, as `<id> <name>`,
#      and referred to as `<id>` alone afterwards -- so the id->name map has to
#      be carried. `fn=` (the function being defined) and `cfn=` (a callee)
#      share ONE namespace, so an id introduced by `fn=` must be recorded too:
#      `malloc` is normally first seen as a callee but `calloc` and `realloc`
#      are first seen as `fn=`, and a map fed only from `cfn=` lines therefore
#      counts the first and misses the other two. (`fl=`/`ob=` and their `c`
#      forms are separate namespaces and must not be mixed in.)
#   2. Cycle/recursion suffixes: callgrind may name a second entry to the same
#      function `malloc'"'"'2`, which is the same allocator.
CG_ALLOC_AWK='
/^(fn|cfn)=/ {
    line = $0
    isc = (substr($0, 1, 1) == "c")
    sub(/^c?fn=/, "", line)
    name = line
    if (match(line, /^\([0-9]+\)/)) {
        id = substr(line, RSTART, RLENGTH)
        rest = substr(line, RSTART + RLENGTH)
        sub(/^ +/, "", rest)
        if (rest != "") { nm[id] = rest }
        name = nm[id]
    }
    sub(/'"'"'[0-9]+$/, "", name)
    cur = isc ? name : ""
    next
}
/^calls=/ {
    if (cur in want) {
        n = $0
        sub(/^calls=/, "", n)
        split(n, f, " ")
        total += f[1]
    }
    cur = ""
    next
}
BEGIN {
    split("malloc calloc realloc aligned_alloc posix_memalign memalign", a, " ")
    for (i in a) want[a[i]] = 1
}
END { print total + 0 }
'

# --self-test: run CG_ALLOC_AWK against a synthetic profile with a known answer.
# The extractor's failure mode is a silent undercount -- an allocator whose name
# it fails to resolve simply contributes nothing, and the total stays plausible
# (the `cfn=`-only version of this was 1,374 low on bench-hash and looked fine),
# so there is nothing for a human to notice. This fixture therefore includes
# every shape that has been got wrong or could be: an id introduced by `fn=` and
# used by `cfn=`, an id introduced by `cfn=`, a recursion-suffixed name, a
# same-numbered id in the FILE namespace (which must not be mistaken for a
# function), and a non-allocator callee.
self_test() {
    local fixture expected got
    fixture=$(printf '%s\n' \
        'events: Ir' \
        'fl=(1) /src/a.rs' \
        'fn=(2) calloc' \
        '1 10' \
        'fn=(3) realloc' \
        '2 10' \
        'fl=(2) /src/not-a-function.rs' \
        'fn=(4) work' \
        '3 10' \
        'cfn=(5) malloc' \
        'calls=7 0' \
        '3 100' \
        'cfn=(2)' \
        'calls=3 0' \
        '3 100' \
        'cfn=(3)' \
        'calls=2 0' \
        '3 100' \
        'cfn=(6) free' \
        'calls=9 0' \
        '3 100' \
        'fn=(7) malloc'\''2' \
        '4 10' \
        'fn=(8) other' \
        '5 10' \
        'cfn=(7)' \
        'calls=5 0' \
        '5 100' \
        'cfn=(9) posix_memalign' \
        'calls=1 0' \
        '5 100')
    expected=18   # malloc 7 + calloc 3 + realloc 2 + malloc'2 5 + posix_memalign 1
    got=$(printf '%s\n' "$fixture" | awk "$CG_ALLOC_AWK")
    if [ "$got" != "$expected" ]; then
        echo "bench-det --self-test: FAILED -- allocator edges summed to $got, expected $expected." >&2
        echo "  The callgrind name map is wrong; every allocation count this script" >&2
        echo "  prints is suspect. See the CG_ALLOC_AWK comment for the two traps." >&2
        return 1
    fi
    # `free` must not be counted, and neither must a file-namespace id collision.
    echo "bench-det --self-test: ok ($got allocator calls in the fixture)"
}

if [ "${1:-}" = --self-test ]; then
    self_test
    exit
fi

# After --self-test, which is pure text processing and must work on a box with no
# valgrind (it runs in the CI checks job, which installs none).
if ! command -v valgrind >/dev/null 2>&1; then
    echo "bench-det: valgrind not found; install it (apt-get install -y valgrind)" >&2
    exit 1
fi

mkdir -p "$(dirname "$CG_OUT")"

# Tells a benchmark it is running under callgrind (~90x native), so one sized
# for the wall-clock series can shrink its input: benchmarks/bench-json-fast-spdx
# decodes 100 records here instead of 727. The instruction series only has to
# be comparable with itself across commits, never with the wall-clock series.
export BENCH_DET=1

measure() { # $1=bench-file; sets $ir and $allocs
    local raw
    ir=NA
    allocs=NA
    rm -f "$CG_OUT"
    # callgrind prints "I   refs:      269,396,968" as its last summary line;
    # that is the whole instruction measurement, so no callgrind_annotate pass
    # is needed. The output file is kept only for the allocation counts.
    raw=$(timeout "$TIMEOUT" valgrind --tool=callgrind --callgrind-out-file="$CG_OUT" \
            "$MUTSU" "$1" 2>&1 >/dev/null |
          sed -n 's/.*I  *refs: *//p' | tr -d ', ') || true
    if [ -n "${raw:-}" ]; then ir=$raw; fi
    # A timed-out or crashed run leaves no usable profile; NA rather than 0, so
    # a failed measurement cannot read as "this benchmark stopped allocating".
    if [ "$ir" != NA ] && [ -s "$CG_OUT" ]; then
        allocs=$(awk "$CG_ALLOC_AWK" "$CG_OUT") || allocs=NA
    fi
    rm -f "$CG_OUT"
}

bytes() { # $1=bench-file -> bytes allocated, or NA
    local n
    n=$(timeout "$TIMEOUT" valgrind --tool=memcheck --error-exitcode=0 \
            "$MUTSU" "$1" 2>&1 >/dev/null |
        sed -n 's/.*total heap usage:.* \([0-9,]*\) bytes allocated.*/\1/p' | tr -d ',') || true
    if [ -n "${n:-}" ]; then printf '%s' "$n"; else printf 'NA'; fi
}

emit() { # $1=row-name $2=bench-file
    local ir allocs
    measure "$2"
    if [ "$WANT_BYTES" = 1 ]; then
        printf '%s\t%s\t%s\t%s\n' "$1" "$ir" "$allocs" "$(bytes "$2")"
    else
        printf '%s\t%s\t%s\n' "$1" "$ir" "$allocs"
    fi
}

# No arguments: the whole suite, which is what CI runs. Named files instead when
# investigating one benchmark by hand -- the allocation column is meant to be the
# acceptance criterion for a slice, and re-measuring the whole suite to check one
# file of it is four minutes for nothing.
if [ "$#" -gt 0 ]; then
    benches=("$@")
else
    benches=(benchmarks/*.raku)
fi

for bench in "${benches[@]}"; do
    name=$(basename "$bench" .raku)
    # Same two series as scripts/bench-ci.sh, under the same names, so a row
    # can be read across both histories: the plain one pins MUTSU_JIT=off as
    # the interpreter baseline, `+jit` is the default configuration.
    MUTSU_JIT=off emit "$name" "$bench"
    MUTSU_JIT=on emit "$name+jit" "$bench"
done
