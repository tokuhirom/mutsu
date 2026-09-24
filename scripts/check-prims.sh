#!/usr/bin/env bash
# Ban on private copies of a primitive's implementation (ADR-0117, ADR-0118).
#
# In Rakudo a method or operator is a thin wrapper over one MoarVM op, so
# `.substr` and `nqp::substr`, or `7 div -2` and `[div] 7, -2`, are one
# routine and cannot disagree. mutsu grew several copies of the same
# primitive instead -- one per layer (method, `nqp::` op, VM opcode,
# reduction fold, TRIR op) -- and they drifted:
#
#   Str  (ADR-0117)  `nqp::chars("\r\n")` was 2 while `.chars` was 1; the nqp
#                    and TRIR layers kept their own codepoint-indexed memos.
#   Int  (ADR-0118)  `[div] 7, -2` was -3 while `7 div -2` was -4, `5.5 +& 3`
#                    was 0 while `[+&] 5.5, 3` was 1, and `$min div -1`
#                    panicked in the one copy that forgot the overflow guard.
#
# Each copy was written for a good local reason (a perf memo, a fast path),
# which is exactly why a comment asking people not to do it would not hold.
# The single homes are `src/builtins/str_prim/` and `src/builtins/arith/`.
# This gate fails the build when a layer grows a private routine again:
#
#   str   In the nqp:: op tables, the VM's nqp call path and TRIR's runtime, a
#         line that walks, cases, normalizes, repeats or searches a string by
#         itself. Call `crate::builtins::str_prim` instead.
#         Opt-out marker: `str-prim: allow`.
#   int   Anywhere outside `src/builtins/arith/` (and the parser/compiler), a
#         hand-written floored integer division or modulus. Call
#         `crate::builtins::{int_div, arith_mod, int_mod_i64}` instead.
#         Opt-out marker: `int-prim: allow`.
#   native In the nqp:: op tables, the VM's nqp call path and TRIR's runtime,
#         a hand-written wrapping int op (`.wrapping_add(` ...). Call
#         `crate::runtime::nqp_native` instead: TRIR's own copy of
#         `bitshiftl_i` once answered differently from the op table's.
#         Opt-out marker: `native-prim: allow`.
#   names The old private copies, banned by name anywhere in src/, so they
#         cannot come back under their own names either.
#
# A marker goes in a comment on the offending line or the line above it,
# ideally with the reason. Comment lines are never flagged.
#
#   scripts/check-prims.sh              # `make check-prims`, a `make test`
#                                       # prerequisite and a CI step
#   scripts/check-prims.sh --self-test  # proves the patterns still match
set -euo pipefail

cd "$(dirname "$0")/.."

# -- str (ADR-0117) --
# The layers that must delegate: every nqp:: op table, the VM's nqp call path,
# and TRIR's runtime. (TRIR's lowering, `src/trir/compile/`, inspects source
# names at compile time, which is not a Str primitive.)
STR_SCOPE='^src/runtime/nqp_[a-z_]*\.rs$|^src/vm/vm_call_nqp\.rs$|^src/trir/[^/]*\.rs$'
STR_RE='\.chars\(\)|\.char_indices\(\)|\.graphemes\(|\.grapheme_indices\(|Vec<char>|\.to_uppercase\(\)|\.to_lowercase\(\)|\.nfc\(\)|\.nfd\(\)|\.nfkc\(\)|\.nfkd\(\)|\.repeat\(|\.rev\(\)\.collect::<String>'

# -- int (ADR-0118) --
# Everything except the home itself and the parser/compiler.
INT_SCOPE='^src/'
INT_EXEMPT='^src/builtins/arith/|^src/parser/|^src/compiler/'
INT_RE='Integer::(div_floor|mod_floor)|[^a-z_](div_floor|mod_floor)\(&'

# -- native (ADR-0118) --
NATIVE_SCOPE="$STR_SCOPE"
NATIVE_EXEMPT='^src/runtime/nqp_native\.rs$'
NATIVE_RE='\.wrapping_(add|sub|mul|neg|abs|shl|shr|rem|div)\('

# -- names --
NAMES_RE='nqp_char_cache|TrCharCache|cached_chars\(|fn shift_(left|right)_(i64|bigint)|fn superscript_(succ|pred)|fn floor_div_i'

# $1 = root, $2 = scope regex, $3 = exempt regex ('' for none), $4 = pattern,
# $5 = opt-out marker. Prints offending `file:line: text` rows.
scan_rule() {
    local root=$1 scope=$2 exempt=$3 re=$4 marker=$5
    (
        cd "$root"
        find src -name '*.rs' | sort | while read -r f; do
            printf '%s\n' "$f" | grep -qE "$scope" || continue
            if [ -n "$exempt" ] && printf '%s\n' "$f" | grep -qE "$exempt"; then
                continue
            fi
            # The pattern goes in through the environment, not `-v`: `-v`
            # processes backslash escapes, and gawk turns `\.` into `.`.
            RE="$re" MARKER="$marker" awk -v f="$f" '
                BEGIN { re = ENVIRON["RE"]; marker = ENVIRON["MARKER"] }
                {
                    line = $0
                    allowed = (index(line, marker) > 0) || (index(prev, marker) > 0)
                    comment = (line ~ /^[ \t]*(\/\/|\*)/)
                    if (!allowed && !comment && line ~ re) print f ":" NR ": " line
                    prev = line
                }' "$f"
        done
    )
}

scan() {
    local root=$1
    scan_rule "$root" "$STR_SCOPE" '' "$STR_RE" 'str-prim: allow' | sed 's/^/[str] /'
    scan_rule "$root" "$INT_SCOPE" "$INT_EXEMPT" "$INT_RE" 'int-prim: allow' | sed 's/^/[int] /'
    scan_rule "$root" "$NATIVE_SCOPE" "$NATIVE_EXEMPT" "$NATIVE_RE" 'native-prim: allow' |
        sed 's/^/[native] /'
    (cd "$root" && grep -rnE "$NAMES_RE" src --include='*.rs' | grep -vE '^[^:]+:[0-9]+: *(//|\*)' || true) |
        sed 's/^/[names] /'
}

self_test() {
    local dir
    dir=$(mktemp -d)
    trap 'rm -rf "$dir"' RETURN
    mkdir -p "$dir/src/runtime" "$dir/src/trir/compile" "$dir/src/builtins/arith" "$dir/src/vm"
    cat >"$dir/src/trir/compile/names.rs" <<'EOF'
let first = name.chars().next();
EOF
    cat >"$dir/src/runtime/nqp_ops_str.rs" <<'EOF'
let v: Vec<char> = s.chars().collect();
let u = s.to_uppercase();
// prose: s.chars() in a comment is fine
let n = name.to_lowercase(); // str-prim: allow (an encoding name)
// str-prim: allow -- codepoints are the result
let cps = s.chars();
EOF
    cat >"$dir/src/trir/x.rs" <<'EOF'
let r = s.repeat(3);
EOF
    cat >"$dir/src/builtins/elsewhere.rs" <<'EOF'
let fine = s.chars().rev().collect::<String>();
let bad = nqp_char_cache::cached_chars(args, 0);
EOF
    cat >"$dir/src/vm/ops.rs" <<'EOF'
let q = num_integer::Integer::div_floor(&a, &b);
let r = mod_floor(&a, &b);
// int-prim: allow -- a calendar month, not Raku's `div`
let m = Integer::div_floor(&months, &12);
fn shift_left_i64(a: i64, b: i64) -> Value { todo() }
EOF
    cat >"$dir/src/builtins/arith/int_ops.rs" <<'EOF'
let q = num_integer::Integer::div_floor(&a, &b);
EOF
    cat >"$dir/src/runtime/nqp_ops.rs" <<'EOF'
let v = a.wrapping_shl(b as u32);
// native-prim: allow
let r = r.wrapping_mul(10);
EOF
    cat >"$dir/src/runtime/nqp_native.rs" <<'EOF'
pub(crate) fn add_i(a: i64, b: i64) -> i64 { a.wrapping_add(b) }
EOF
    local got
    got=$(scan "$dir" | grep -c '' || true)
    # str: nqp_ops_str.rs lines 1-2 and trir/x.rs line 1 (not trir/compile,
    # which is out of scope); int: vm/ops.rs lines 1-2 (not the marked line 4,
    # nor the arith home); native: nqp_ops.rs line 1 (not the marked line 3,
    # nor nqp_native.rs); names: elsewhere.rs line 2 and vm/ops.rs line 5.
    [ "$got" = "8" ] || {
        echo "check-prims: self-test expected 8 hits, got $got:" >&2
        scan "$dir" >&2
        return 1
    }
    echo "check-prims: self-test ok"
}

if [ "${1:-}" = "--self-test" ]; then
    self_test
    exit 0
fi

hits=$(scan .)
if [ -n "$hits" ]; then
    echo "check-prims: a private copy of a primitive:" >&2
    echo "$hits" | sed 's/^/  /' >&2
    cat >&2 <<'MSG'

  Every layer (method, nqp:: op, VM opcode, reduction fold, TRIR op) must call
  the ONE routine for a primitive:

      [str] crate::builtins::str_prim::{chars, nqp_substr, index, rindex,
                nqp_eqat, eq_at, char_at, find_char, flip, repeat, concat,
                normalize, Fold, ...}                          (ADR-0117)
            crate::builtins::unicode::{grapheme_uppercase, ...}
      [int] crate::builtins::{int_div, arith_mod, int_mod_i64, int_bitop,
                int_shift_left, int_shift_right, int_negate, int_abs,
                value_succ, value_pred}                        (ADR-0118)
      [native] crate::runtime::nqp_native::{add_i, shl_i, div_i, mod_i, ...}
                                                               (ADR-0118)
      [names] the old private copies must not come back.

  If a line is genuinely not that primitive, mark it with a
  `// str-prim: allow` / `// int-prim: allow` / `// native-prim: allow`
  comment (with the reason)
  on that line or the line above.
MSG
    exit 1
fi
echo "check-prims: ok"
