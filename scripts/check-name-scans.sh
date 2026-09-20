#!/usr/bin/env bash
# Shrinking ratchet on run-time package-name string surgery (issue #8899).
#
# A package-qualified name is DERIVED: `Pkg::thing` is a function of the
# package and the thing, both of which the caller already holds, and whether a
# name is qualified at all is a function of its text. Building one with
# `format!("{pkg}::{name}")`, or asking with `name.contains("::")`, does at run
# time what the source decided once -- and because a name is a compile-time
# constant, "once" means at parse time.
#
# This is the same finding `scripts/check-magic-keys.sh` records for
# `__mutsu_*` metadata keys, in a different namespace and measurably larger.
# Profiling one `JSON::Fast` decode (#8898):
#
#   resolve_type_in_current_package   5.11% inclusive (0.48% in `format!` alone)
#   running_module_bareword           1.54%  (+0.77% `format!`)
#   set_our_var                       1.23%
#   resolve_type_name_for_owner       1.16%  (+0.45% `format!`)
#
# `StrSearcher::new` -- the `"::"` searches -- was constructed 1,390,603 times
# in a ten-decode run, and its top callers are exactly those functions.
#
# src/qualified.rs is the memoizing constructor these sites are supposed to be
# using: `qualified()` builds a pair once, `package_ancestors()` walks the
# enclosing chain without allocating, `is_qualified()` and
# `is_global_package()` classify once.
#
# Counts may go DOWN, never up. When your change lowers one, re-cut with
#
#   scripts/check-name-scans.sh --update
#
# and commit the new baseline with it. `--self-test` proves the patterns still
# match what this prose says they match.
#
# EXEMPT, deliberately:
#   src/parser/     deciding what a name is from its text is the job
#   src/compiler/   likewise, and it runs once per program, not per execution
#   src/symbol.rs   the interner itself
#   src/qualified.rs, src/runtime/meta_ns.rs
#                   the memoizing constructors; their own `format!` is the
#                   one that is allowed
set -euo pipefail

cd "$(dirname "$0")/.."

BASELINE_FILE=scripts/name-scans-baseline.txt

# A `format!` literal that joins two interpolations with `::` -- the
# hand-built qualified name. Matches both the inline (`"{pkg}::{name}"`) and
# positional (`"{}::{}"`) spellings.
QUALIFY_RE='"\{[A-Za-z_][A-Za-z0-9_]*\}::\{|"\{\}::\{\}"'
# The package classification done by string compare rather than by symbol id.
GLOBAL_RE='== *"GLOBAL"|!= *"GLOBAL"'
# Any `"::"` string surgery: classification, splitting, or walking the chain.
SCAN_RE='\.(contains|split|rsplit|rsplit_once|split_once|splitn|rsplitn|find|rfind|starts_with|ends_with|strip_prefix|strip_suffix|matches)\("::"\)|has_double_colon\('

exempt() {
    grep -vE '^src/(parser|compiler)/|^src/(symbol|qualified)\.rs:|^src/runtime/meta_ns\.rs:'
}
# Whole-line comments only: prose that quotes a pattern is not a call site, but
# appending a trailing `// ...` to a real one must never silence the gate.
no_comments() {
    grep -vE '^[^:]+:[0-9]+: *(//|\*|//!)'
}

sites_for() { # $1 = regex
    { grep -rEn "$1" src --include='*.rs' | no_comments | exempt; } || true
}

count_for() { sites_for "$1" | grep -c '' || true; }

self_test() {
    local dir
    dir=$(mktemp -d)
    trap 'rm -rf "$dir"' RETURN
    mkdir -p "$dir/src/parser" "$dir/src/runtime"
    cat >"$dir/src/runtime/a.rs" <<'RS'
let k = format!("{pkg}::{name}");
let k2 = format!("{}::{}", pkg, name);
if cur == "GLOBAL" { }
if cur != "GLOBAL" { }
if name.contains("::") { }
match pkg.rsplit_once("::") { _ => {} }
if has_double_colon(name) { }
// format!("{pkg}::{name}") in prose does not count
let ok = format!("{pkg}_{name}");
let ok2 = name.contains("::x");
RS
    cat >"$dir/src/parser/b.rs" <<'RS'
let k = format!("{pkg}::{name}");
if name.contains("::") { }
RS
    local got want
    got="$(cd "$dir" && { { grep -rEn "$QUALIFY_RE" src --include='*.rs' || true; } | grep -vE '^[^:]+:[0-9]+: *(//|\*|//!)' | grep -vE '^src/(parser|compiler)/' | grep -c '' || true; })"
    want=2
    [ "$got" = "$want" ] || { echo "self-test: qualify expected $want, got $got" >&2; return 1; }
    got="$(cd "$dir" && { { grep -rEn "$GLOBAL_RE" src --include='*.rs' || true; } | grep -c '' || true; })"
    [ "$got" = "2" ] || { echo "self-test: global-cmp expected 2, got $got" >&2; return 1; }
    got="$(cd "$dir" && { { grep -rEn "$SCAN_RE" src --include='*.rs' || true; } | grep -vE '^[^:]+:[0-9]+: *(//|\*|//!)' | grep -vE '^src/(parser|compiler)/' | grep -c '' || true; })"
    [ "$got" = "3" ] || { echo "self-test: scan expected 3, got $got" >&2; return 1; }
    echo "check-name-scans: self-test ok"
}

if [ "${1:-}" = "--self-test" ]; then
    self_test
    exit 0
fi

qualify=$(count_for "$QUALIFY_RE")
global_cmp=$(count_for "$GLOBAL_RE")
scan=$(count_for "$SCAN_RE")

if [ "${1:-}" = "--update" ]; then
    printf 'qualify %s\nglobal-cmp %s\nscan %s\n' "$qualify" "$global_cmp" "$scan" >"$BASELINE_FILE"
    echo "name-scans baseline updated: qualify=$qualify global-cmp=$global_cmp scan=$scan"
    exit 0
fi

failed=0
check() { # $1 = label, $2 = current
    local base
    base=$(awk -v k="$1" '$1 == k { print $2 }' "$BASELINE_FILE")
    if [ -z "$base" ]; then
        echo "check-name-scans: no baseline entry for '$1'" >&2
        failed=1
        return
    fi
    if [ "$2" -gt "$base" ]; then
        echo "check-name-scans: $1 rose from $base to $2" >&2
        failed=1
    elif [ "$2" -lt "$base" ]; then
        echo "check-name-scans: $1 fell from $base to $2 -- re-cut the baseline:" >&2
        echo "  scripts/check-name-scans.sh --update" >&2
        failed=1
    else
        echo "check-name-scans: $1 $2 (baseline $base)"
    fi
}

check qualify "$qualify"
check global-cmp "$global_cmp"
check scan "$scan"

if [ "$failed" -ne 0 ]; then
    cat >&2 <<'MSG'

  Build a package-qualified name with src/qualified.rs, not by hand:

      qualified(pkg_sym, name_sym)      -> Symbol, memoized per pair;
                                           `.as_str()` for the &str APIs
      package_ancestors(pkg_sym)        -> the enclosing chain, no allocation
      is_qualified(name_sym)            -> classified once per symbol
      is_global_package(pkg_sym)        -> two id compares, no String clone

  src/parser/ and src/compiler/ are exempt: deciding what a name is from its
  text is their job, and doing it there instead of per execution is the point.

  See https://github.com/tokuhirom/mutsu/issues/8899.
MSG
    exit 1
fi
