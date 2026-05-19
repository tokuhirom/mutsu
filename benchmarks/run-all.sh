#!/bin/bash
# Run all benchmarks and report times
# Usage: ./benchmarks/run-all.sh [mutsu-binary] [raku-binary]

MUTSU=${1:-target/release/mutsu}
RAKU=${2:-raku}
TIMEOUT=60

printf "%-20s %10s %10s %8s\n" "Benchmark" "mutsu(s)" "raku(s)" "ratio"
printf "%-20s %10s %10s %8s\n" "---------" "--------" "-------" "-----"

for bench in benchmarks/*.raku; do
    name=$(basename "$bench" .raku)

    # Run mutsu
    mutsu_time=$( { time timeout $TIMEOUT "$MUTSU" "$bench" > /dev/null 2>&1; } 2>&1 | grep real | awk '{print $2}' | sed 's/[ms]/ /g' | awk '{printf "%.3f", $1*60+$2}' )

    # Run raku
    raku_time=$( { time timeout $TIMEOUT "$RAKU" "$bench" > /dev/null 2>&1; } 2>&1 | grep real | awk '{print $2}' | sed 's/[ms]/ /g' | awk '{printf "%.3f", $1*60+$2}' )

    if [ -n "$mutsu_time" ] && [ -n "$raku_time" ] && [ "$raku_time" != "0.000" ]; then
        ratio=$(echo "$mutsu_time $raku_time" | awk '{printf "%.1fx", $1/$2}')
    else
        ratio="N/A"
    fi

    printf "%-20s %10s %10s %8s\n" "$name" "${mutsu_time}s" "${raku_time}s" "$ratio"
done
