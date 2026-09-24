#!/usr/bin/env bash
# Ban on private copies of the Str primitives (ADR-0117).
#
# Rakudo's `Str` methods are thin wrappers over the MoarVM string ops, so
# `.substr` and `nqp::substr` are one routine and cannot disagree. mutsu had
# three copies instead -- the `Str` methods (grapheme-indexed), the `nqp::` ops
# (codepoint-indexed, on a private `Vec<char>` memo, `nqp_char_cache.rs`) and
# TRIR's typed string ops (codepoint-indexed, on a second private memo,
# `TrCharCache`) -- and they drifted: `nqp::chars("\r\n")` was 2, `nqp::index`
# reported codepoint offsets, `nqp::flip` reversed codepoints. Each copy was
# written for a good local reason (a perf memo, a fast path), which is exactly
# why a comment asking people not to do it would not hold.
#
# `src/builtins/str_prim/` is now the single home, and this gate keeps the
# nqp/TRIR/VM-op layers from growing a private string routine again. In the
# files below, a line that walks, cases, normalizes, repeats or searches a
# string by itself is a build failure: call `crate::builtins::str_prim` (or
# `grapheme_index` / `unicode::grapheme_*`) instead, which is the routine the
# matching `Str` method already uses.
#
# A line that really is not a Str primitive (parsing digits, lowercasing an
# encoding NAME, turning a string into its codepoints for `nqp::strtocodes`)
# carries `str-prim: allow` in a comment on that line or the line above it,
# ideally with the reason. Two names are banned everywhere in src/: the old
# private memos must not come back under their own names either.
#
#   scripts/check-str-prims.sh              # `make check-str-prims`, a
#                                           # `make test` prerequisite and a CI step
#   scripts/check-str-prims.sh --self-test  # proves the patterns still match
set -euo pipefail

cd "$(dirname "$0")/.."

# The layers that must delegate: every nqp:: op table, the VM's nqp call path,
# and TRIR's runtime. (TRIR's lowering, `src/trir/compile/`, inspects source
# names at compile time, which is not a Str primitive.)
SCOPE_RE='^src/runtime/nqp_[a-z_]*\.rs$|^src/vm/vm_call_nqp\.rs$|^src/trir/[^/]*\.rs$'

# A string walked, cased, normalized, repeated or searched by hand.
PRIM_RE='\.chars\(\)|\.char_indices\(\)|\.graphemes\(|\.grapheme_indices\(|Vec<char>|\.to_uppercase\(\)|\.to_lowercase\(\)|\.nfc\(\)|\.nfd\(\)|\.nfkc\(\)|\.nfkd\(\)|\.repeat\(|\.rev\(\)\.collect::<String>'

# Private per-layer string memos, banned by name anywhere.
MEMO_RE='nqp_char_cache|TrCharCache|cached_chars\('

# $1 = root dir. Prints offending `file:line: text` rows.
scan() {
    local root=$1
    (
        cd "$root"
        find src -name '*.rs' | sort | while read -r f; do
            if printf '%s\n' "$f" | grep -qE "$SCOPE_RE"; then
                # The pattern goes in through the environment, not `-v`: `-v`
                # processes backslash escapes, and gawk turns `\.` into `.`.
                PRIM_RE="$PRIM_RE" awk -v f="$f" '
                    BEGIN { re = ENVIRON["PRIM_RE"] }
                    {
                        line = $0
                        allowed = (line ~ /str-prim: allow/) || (prev ~ /str-prim: allow/)
                        comment = (line ~ /^[ \t]*(\/\/|\*)/)
                        if (!allowed && !comment && line ~ re) print f ":" NR ": " line
                        prev = line
                    }' "$f"
            fi
        done
        grep -rnE "$MEMO_RE" src --include='*.rs' | grep -vE '^[^:]+:[0-9]+: *(//|\*)' || true
    )
}

self_test() {
    local dir
    dir=$(mktemp -d)
    trap 'rm -rf "$dir"' RETURN
    mkdir -p "$dir/src/runtime" "$dir/src/trir/compile" "$dir/src/builtins"
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
    local got
    got=$(scan "$dir" | grep -c '' || true)
    # nqp_ops_str.rs lines 1 and 2, trir/x.rs line 1, elsewhere.rs line 2 --
    # not trir/compile/names.rs, which is out of scope.
    [ "$got" = "4" ] || {
        echo "check-str-prims: self-test expected 4 hits, got $got:" >&2
        scan "$dir" >&2
        return 1
    }
    echo "check-str-prims: self-test ok"
}

if [ "${1:-}" = "--self-test" ]; then
    self_test
    exit 0
fi

hits=$(scan .)
if [ -n "$hits" ]; then
    echo "check-str-prims: a private copy of a Str primitive:" >&2
    echo "$hits" | sed 's/^/  /' >&2
    cat >&2 <<'MSG'

  The nqp:: ops, TRIR and the VM's nqp path must call the SAME routine the
  matching Str method uses (ADR-0117):

      crate::builtins::str_prim::{chars, nqp_substr, index, rindex, nqp_eqat,
          eq_at, char_at, find_char, flip, repeat, concat, normalize, Fold, ...}
      crate::builtins::unicode::{grapheme_uppercase, grapheme_lowercase, ...}

  If the line is genuinely not a string primitive, mark it with a
  `// str-prim: allow (<reason>)` comment on that line or the line above.
MSG
    exit 1
fi
echo "check-str-prims: ok"
