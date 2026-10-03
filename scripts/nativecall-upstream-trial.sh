#!/usr/bin/env bash
# How far does the vendored upstream NativeCall run on mutsu today?
#
# ADR-11203 / #11203: `use NativeCall` and `use NativeCall::Types` are still
# intercepted by name and served by the native provider, so the vendored files
# in modules/Rakudo-Core/lib/ cannot be loaded under their own names yet. This
# copies them to tmp/nativecall-trial/ with the namespace renamed `NativeCall`
# -> `UNC` (a mechanical sed; nothing else changes) and runs one probe per
# step, each in its own process, so one failure does not hide the next.
#
# Usage: scripts/nativecall-upstream-trial.sh [path/to/mutsu]
#        (default: $MUTSU_BIN, else target/debug/mutsu)
# Exit status: 0 when every step passes, 1 otherwise.
set -u

root=$(cd "$(dirname "$0")/.." && pwd)
mutsu=${1:-${MUTSU_BIN:-$root/target/debug/mutsu}}
src=$root/modules/Rakudo-Core/lib
out=$root/tmp/nativecall-trial
lib=$out/lib

if [[ ! -x $mutsu ]]; then
    echo "nativecall-upstream-trial: no mutsu binary at $mutsu (cargo build first)" >&2
    exit 2
fi

rm -rf "$out"
mkdir -p "$lib/UNC/Compiler"
for f in NativeCall.rakumod:UNC.rakumod \
         NativeCall/Types.rakumod:UNC/Types.rakumod \
         NativeCall/Dispatcher.rakumod:UNC/Dispatcher.rakumod \
         NativeCall/Compiler/GNU.rakumod:UNC/Compiler/GNU.rakumod \
         NativeCall/Compiler/MSVC.rakumod:UNC/Compiler/MSVC.rakumod; do
    sed 's/NativeCall\b/UNC/g' "$src/${f%%:*}" > "$lib/${f##*:}"
done

# Each step: a label and a program. Later steps assume the earlier ones pass,
# so the first failure is the current frontier of #11203.
steps=(
    "load UNC::Compiler::GNU|use UNC::Compiler::GNU; print 'ok'"
    "load UNC::Types|use UNC::Types; print 'ok'"
    "Types: native type traits|use UNC::Types; print UNC::Types::ulong.^unsigned == 1 && UNC::Types::long.^nativesize == -4 ?? 'ok' !! 'wrong'"
    "Types: Pointer.new|use UNC::Types; print UNC::Types::Pointer.new.raku eq 'UNC::Types::Pointer.new(0)' ?? 'ok' !! UNC::Types::Pointer.new.raku"
    "Types: CArray[int32] elements|use UNC::Types; my \$a = UNC::Types::CArray[int32].new(1,2,3); print \$a[1] == 2 && \$a.elems == 3 ?? 'ok' !! 'wrong'"
    "Types: CArray[int32] write|use UNC::Types; my \$a = UNC::Types::CArray[int32].new(1,2,3); \$a[0] = 7; print \$a[0] == 7 ?? 'ok' !! 'wrong'"
    "load UNC|use UNC; print 'ok'"
    "UNC: nativesizeof|use UNC; print nativesizeof(int32) == 4 ?? 'ok' !! 'wrong'"
    "UNC: is native strlen|use UNC; sub strlen(Str --> size_t) is native {*}; print strlen('hello') == 5 ?? 'ok' !! 'wrong'"
    "UNC: CStruct round trip|use UNC; class TM is repr<CStruct> { has int32 \$.a; has int32 \$.b }; my \$t = TM.new(a => 1, b => 2); print \$t.b == 2 && nativesizeof(TM) == 8 ?? 'ok' !! 'wrong'"
)

fail=0
for step in "${steps[@]}"; do
    label=${step%%|*}
    code=${step#*|}
    result=$(cd "$out" && timeout 60 "$mutsu" -I "$lib" -e "$code" 2>&1)
    status=$?
    if [[ $status -eq 0 && $result == ok ]]; then
        printf 'PASS  %s\n' "$label"
    else
        fail=1
        printf 'FAIL  %s\n' "$label"
        printf '%s\n' "$result" | head -n 6 | sed 's/^/        /'
    fi
done
exit $fail
