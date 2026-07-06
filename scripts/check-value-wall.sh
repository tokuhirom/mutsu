#!/usr/bin/env bash
# Ratchet for the 3b-0 Value API wall (docs/nanbox-3b0-api-wall.md).
#
# Counts direct `Value::<Variant>` mentions OUTSIDE src/value/ (the
# representation module). The count must only go down: this script fails when
# it rises above the recorded baseline, so no new direct variant use can land
# while the NaN-boxing migration is in flight.
#
# When your change lowers the count, run `scripts/check-value-wall.sh --update`
# and commit the new baseline together with it.
set -euo pipefail

cd "$(dirname "$0")/.."

BASELINE_FILE=scripts/value-wall-baseline.txt

# All 54 Value enum variants (src/value/mod.rs). `\bValue::` keeps
# `EnumValue::Int` etc. from matching; wall constructors/consts are
# lowercase or not variant names, so they never match.
VARIANTS='Int|BigInt|Num|Str|Bool|Range|RangeExcl|RangeExclStart|RangeExclBoth|GenericRange|Array|Hash|Rat|FatRat|BigRat|Complex|Set|Bag|Mix|CompUnitDepSpec|Package|Routine|Pair|ValuePair|Enum|Regex|RegexWithAdverbs|Sub|WeakSub|Instance|Junction|Seq|HyperSeq|RaceSeq|Slip|LazyList|Version|Promise|Channel|Nil|Whatever|HyperWhatever|Mixin|Capture|Uni|Proxy|ParametricRole|CustomType|CustomTypeInstance|Scalar|ContainerRef|LazyThunk|LazyIoLines|HashEntryRef'

current=$(grep -rEo "\bValue::(${VARIANTS})\b" src --include='*.rs' \
    | grep -v '^src/value/' | wc -l)

if [[ "${1:-}" == "--update" ]]; then
    echo "$current" > "$BASELINE_FILE"
    echo "value-wall baseline updated: $current"
    exit 0
fi

baseline=$(cat "$BASELINE_FILE")

if (( current > baseline )); then
    echo "value-wall ratchet FAILED: $current direct Value:: variant uses outside src/value/ (baseline: $baseline)." >&2
    echo "New code must use the wall API (Value::view()/as_*/constructors) — see docs/nanbox-3b0-api-wall.md." >&2
    echo "Offending sites are findable with:" >&2
    echo "  grep -rnE '\\bValue::(${VARIANTS%%|*}|...)\\b' src --include='*.rs' | grep -v '^src/value/'" >&2
    exit 1
elif (( current < baseline )); then
    echo "value-wall: $current direct uses (baseline $baseline) — lower the baseline: scripts/check-value-wall.sh --update"
else
    echo "value-wall: $current direct uses (== baseline)"
fi
