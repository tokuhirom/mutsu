#!/usr/bin/env bash
# Performance benchmark suite: mutsu vs raku
# Usage: ./benchmarks/run-benchmarks.sh [--mutsu-only|--raku-only]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
MUTSU="$REPO_DIR/target/release/mutsu"

if [ ! -f "$MUTSU" ]; then
    echo "ERROR: Release build not found at $MUTSU"
    echo "Run: cargo build --release"
    exit 1
fi

if ! command -v raku &>/dev/null; then
    echo "WARNING: raku not found, will only run mutsu benchmarks"
    RAKU_AVAILABLE=0
else
    RAKU_AVAILABLE=1
fi

MODE="both"
if [ "${1:-}" = "--mutsu-only" ]; then
    MODE="mutsu"
elif [ "${1:-}" = "--raku-only" ]; then
    MODE="raku"
fi

BENCHMARKS=(
    "bench-startup.raku:Startup (say hello)"
    "bench-fib.raku:Fibonacci (recursive, fib(1..25))"
    "bench-string.raku:String ops (concat/split/regex)"
    "bench-array.raku:Array ops (push/map/grep/sort)"
    "bench-hash.raku:Hash ops (insert/lookup/delete)"
    "bench-class.raku:OOP (classes/methods/inheritance)"
)

# Measure wall-clock time of a command, return seconds with 3 decimal places
measure() {
    local cmd="$1"
    local file="$2"
    # Use bash TIMEFORMAT for sub-second precision
    local result
    result=$( { TIMEFORMAT='%3R'; time $cmd "$file" > /dev/null 2>&1; } 2>&1 )
    echo "$result"
}

# Print header
echo ""
echo "## mutsu Performance Benchmarks"
echo ""
echo "| Benchmark | mutsu (s) | raku (s) | Ratio (mutsu/raku) |"
echo "|-----------|-----------|----------|--------------------|"

for entry in "${BENCHMARKS[@]}"; do
    IFS=':' read -r file label <<< "$entry"
    filepath="$SCRIPT_DIR/$file"

    mutsu_time="-"
    raku_time="-"
    ratio="-"

    if [ "$MODE" != "raku" ]; then
        mutsu_time=$(measure "$MUTSU" "$filepath")
    fi

    if [ "$MODE" != "mutsu" ] && [ "$RAKU_AVAILABLE" = "1" ]; then
        raku_time=$(measure "raku" "$filepath")
    fi

    # Calculate ratio if both times are available
    if [ "$mutsu_time" != "-" ] && [ "$raku_time" != "-" ]; then
        ratio=$(awk "BEGIN { printf \"%.2f\", $mutsu_time / $raku_time }")
        ratio="${ratio}x"
    fi

    printf "| %-37s | %9s | %8s | %18s |\n" "$label" "$mutsu_time" "$raku_time" "$ratio"
done

echo ""
echo "_Lower is better. Ratio < 1.0 means mutsu is faster._"
echo ""
