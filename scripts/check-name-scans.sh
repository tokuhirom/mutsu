#!/usr/bin/env bash
# Ban on run-time package-name string surgery (issues #8899, #11507).
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
# A BAN, NOT A RATCHET (#11507). This started as a ratchet over 577 sites
# (#8899), with a per-file frozen baseline so the shrinking PRs could run in
# parallel. #11507 took every counter to zero, the baseline file went with
# it, and what is left is the plain rule, as for `check-magic-keys`: there is
# no hand-built form any more, so a new one is a build failure, not a number
# to compare. A caller that holds only a name's text asks
# `qualified::known_symbol` / `is_qualified_str` (a lookup, interning once).
#
# EXEMPT, deliberately:
#   src/parser/     deciding what a name is from its text is the job
#   src/compiler/   likewise, and it runs once per program, not per execution
#   src/symbol.rs   the interner itself
#   src/qualified_tail_index.rs
#                   the interner's per-symbol member index, run once per
#                   interned name from `Symbol::intern_global`
#   src/qualified.rs, src/qualified/, src/meta_ns.rs
#                   the memoizing constructors; their own `format!` is the
#                   one that is allowed
#   src/str_scan.rs the byte-scan primitive `has_double_colon` itself and its
#                   parity tests against `str::contains`/`rsplit_once`; every
#                   CALL of it elsewhere still counts
set -euo pipefail

cd "$(dirname "$0")/.."

# A `format!` literal that joins two interpolations with `::` -- the
# hand-built qualified name. Matches both the inline (`"{pkg}::{name}"`) and
# positional (`"{}::{}"`) spellings.
QUALIFY_RE='"\{[A-Za-z_][A-Za-z0-9_]*\}::\{|"\{\}::\{\}"'
# The package classification done by string compare rather than by symbol id.
GLOBAL_RE='== *"GLOBAL"|!= *"GLOBAL"'
# Any `"::"` string surgery: classification, splitting, or walking the chain.
SCAN_RE='\.(contains|split|rsplit|rsplit_once|split_once|splitn|rsplitn|find|rfind|starts_with|ends_with|strip_prefix|strip_suffix|matches)\("::"\)|has_double_colon\('

exempt() {
    grep -vE '^src/(parser|compiler|qualified)/|^src/(symbol|qualified|qualified_tail_index|str_scan)\.rs:|^src/meta_ns\.rs:'
}
# Whole-line comments only: prose that quotes a pattern is not a call site, but
# appending a trailing `// ...` to a real one must never silence the gate.
no_comments() {
    grep -vE '^[^:]+:[0-9]+: *(//|\*|//!)'
}

sites_for() { # $1 = regex; run from the tree root to scan
    { grep -rEn "$1" src --include='*.rs' | no_comments | exempt; } || true
}

# Every site, as `path:line: text`, across all three patterns. Run from the
# tree root to scan.
all_sites() {
    {
        sites_for "$QUALIFY_RE"
        sites_for "$GLOBAL_RE"
        sites_for "$SCAN_RE"
    } | LC_ALL=C sort -u
}

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
    local got
    got="$(cd "$dir" && all_sites | wc -l | tr -d ' ')"
    [ "$got" = 7 ] || { echo "self-test: expected 7 sites in src/runtime/a.rs, got $got" >&2; return 1; }
    if (cd "$dir" && all_sites | grep -q '^src/parser/'); then
        echo "self-test: src/parser/ must be exempt" >&2
        return 1
    fi
    rm "$dir/src/runtime/a.rs"
    got="$(cd "$dir" && all_sites | wc -l | tr -d ' ')"
    [ "$got" = 0 ] || { echo "self-test: a clean tree must report no site, got $got" >&2; return 1; }
    echo "check-name-scans: self-test ok"
}

case "${1:-}" in
--self-test)
    self_test
    exit 0
    ;;
esac

sites="$(all_sites)"
if [ -n "$sites" ]; then
    printf '%s\n' "$sites" >&2
    cat >&2 <<'MSG'

  Build a package-qualified name with src/qualified.rs, not by hand:

      qualified(pkg_sym, name_sym)      -> Symbol, memoized per pair;
                                           `.as_str()` for the &str APIs
      qualified_text(pkg, name)         -> the same, from text or symbols
      package_ancestors(pkg_sym)        -> the enclosing chain, no allocation
      split_qualified / last_segment    -> the `rsplit_once("::")` forms
      is_qualified(name_sym)            -> classified once per symbol
      is_qualified_str(name)            -> the same for a text-only caller
      is_global_package(pkg_sym)        -> two id compares, no String clone

  src/parser/ and src/compiler/ are exempt: deciding what a name is from its
  text is their job, and doing it there instead of per execution is the point.
  See https://github.com/tokuhirom/mutsu/issues/11507.
MSG
    exit 1
fi
echo "check-name-scans: no run-time qualified-name string surgery"
