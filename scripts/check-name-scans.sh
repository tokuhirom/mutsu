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
# THE BASELINE IS PER FILE AND FROZEN (#11507). scripts/name-scans-baseline.txt
# holds one row per file: `<path> <qualify> <global-cmp> <scan>`. The check
# fails only when one counter of one file RISES above its row; a file with no
# row is allowed zero. A drop needs no re-cut, so the PRs that shrink the
# counts edit no shared file and cannot conflict with each other here. (A
# single total per counter could not do that: either every shrinking PR
# re-cut it, or a drop in one file would silently pay for a new site in
# another.) A file that has fallen below its row is reported as slack; tighten
# all rows to the current counts with
#
#   scripts/check-name-scans.sh --update
#
# whenever it is convenient -- it is optional, and best done alone. Adding a
# site, or moving sites into a file that has no row (a split), means editing
# the row by hand, which a reviewer sees. `--self-test` proves the patterns
# still match what this prose says they match, and that the per-file
# comparison has the teeth described above.
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
    grep -vE '^src/(parser|compiler|qualified)/|^src/(symbol|qualified|qualified_tail_index)\.rs:|^src/meta_ns\.rs:'
}
# Whole-line comments only: prose that quotes a pattern is not a call site, but
# appending a trailing `// ...` to a real one must never silence the gate.
no_comments() {
    grep -vE '^[^:]+:[0-9]+: *(//|\*|//!)'
}

sites_for() { # $1 = regex; run from the tree root to scan
    { grep -rEn "$1" src --include='*.rs' | no_comments | exempt; } || true
}

# One line per file with any site: `<path> <qualify> <global-cmp> <scan>`,
# sorted by path. Run from the tree root to scan.
current_counts() {
    {
        sites_for "$QUALIFY_RE" | sed 's/^\([^:]*\):.*/qualify \1/'
        sites_for "$GLOBAL_RE" | sed 's/^\([^:]*\):.*/global \1/'
        sites_for "$SCAN_RE" | sed 's/^\([^:]*\):.*/scan \1/'
    } | awk '
        { n[$2, $1]++; files[$2] = 1 }
        END {
            for (f in files)
                printf "%s %d %d %d\n", f, n[f, "qualify"], n[f, "global"], n[f, "scan"]
        }' | LC_ALL=C sort
}

# $1 = baseline file, stdin = current_counts output. Prints the report; exits
# 1 when any counter of any file rose above its row.
compare() {
    awk -v baseline="$1" '
        BEGIN {
            label[1] = "qualify"; label[2] = "global-cmp"; label[3] = "scan"
            while ((getline line < baseline) > 0) {
                sub(/#.*/, "", line)
                if (split(line, f, " ") == 0) continue
                if (split(line, f, " ") != 4) {
                    printf "check-name-scans: malformed baseline row: %s\n", line > "/dev/stderr"
                    bad = 1
                    continue
                }
                base[f[1]] = 1
                for (i = 1; i <= 3; i++) {
                    b[f[1], i] = f[i + 1]
                    btotal[i] += f[i + 1]
                }
            }
        }
        {
            seen[$1] = 1
            for (i = 1; i <= 3; i++) {
                cur = $(i + 1); allowed = ($1 in base) ? b[$1, i] : 0
                total[i] += cur
                if (cur > allowed) {
                    printf "check-name-scans: %s: %s rose from %d to %d\n", $1, label[i], allowed, cur > "/dev/stderr"
                    bad = 1
                } else if (cur < allowed) {
                    slack++
                }
            }
        }
        END {
            for (p in base)
                if (!(p in seen))
                    for (i = 1; i <= 3; i++)
                        if (b[p, i] > 0) slack++
            for (i = 1; i <= 3; i++)
                printf "check-name-scans: %s %d (baseline %d)\n", label[i], total[i], btotal[i]
            if (slack > 0 && !bad)
                printf "check-name-scans: %d per-file counter(s) below their row; optional re-cut: scripts/check-name-scans.sh --update\n", slack
            exit bad
        }'
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
    cat >"$dir/src/runtime/c.rs" <<'RS'
if name.contains("::") { }
RS
    local got want
    got="$(cd "$dir" && current_counts)"
    want="src/runtime/a.rs 2 2 3
src/runtime/c.rs 0 0 1"
    [ "$got" = "$want" ] || { printf 'self-test: counts expected\n%s\ngot\n%s\n' "$want" "$got" >&2; return 1; }

    # Equal to the baseline: passes.
    printf '# comment\nsrc/runtime/a.rs 2 2 3\nsrc/runtime/c.rs 0 0 1\n' >"$dir/base"
    (cd "$dir" && current_counts | compare base >/dev/null 2>&1) ||
        { echo "self-test: an unchanged tree must pass" >&2; return 1; }
    # A drop passes without a re-cut, including a file whose row is now stale.
    printf 'src/runtime/a.rs 5 2 3\nsrc/runtime/c.rs 0 0 4\nsrc/runtime/gone.rs 1 0 0\n' >"$dir/base"
    (cd "$dir" && current_counts | compare base >/dev/null 2>&1) ||
        { echo "self-test: a drop must pass" >&2; return 1; }
    # A rise in one file fails even when another file's drop keeps the total down.
    printf 'src/runtime/a.rs 2 2 9\nsrc/runtime/c.rs 0 0 0\n' >"$dir/base"
    if (cd "$dir" && current_counts | compare base >/dev/null 2>&1); then
        echo "self-test: a per-file rise must fail even when the total fell" >&2
        return 1
    fi
    # A file with no row is allowed zero.
    printf 'src/runtime/a.rs 2 2 3\n' >"$dir/base"
    if (cd "$dir" && current_counts | compare base >/dev/null 2>&1); then
        echo "self-test: a file without a row must be allowed zero" >&2
        return 1
    fi
    echo "check-name-scans: self-test ok"
}

write_baseline() {
    {
        echo "# Run-time package-name string surgery, per file (scripts/check-name-scans.sh,"
        echo "# #8899, #11507). Columns: <path> <qualify> <global-cmp> <scan>."
        echo "# A counter may not rise above its row; a file without a row is allowed zero."
        echo "# A drop needs no re-cut. Tighten every row at once (optional) with"
        echo "#   scripts/check-name-scans.sh --update"
        echo
        current_counts | awk '{ printf "%-56s %4d %4d %4d\n", $1, $2, $3, $4 }'
    } >"$BASELINE_FILE"
}

case "${1:-}" in
--self-test)
    self_test
    exit 0
    ;;
--update)
    write_baseline
    echo "name-scans baseline re-cut: $(grep -vc '^#\|^$' "$BASELINE_FILE") files"
    exit 0
    ;;
esac

if ! current_counts | compare "$BASELINE_FILE"; then
    cat >&2 <<'MSG'

  Build a package-qualified name with src/qualified.rs, not by hand:

      qualified(pkg_sym, name_sym)      -> Symbol, memoized per pair;
                                           `.as_str()` for the &str APIs
      package_ancestors(pkg_sym)        -> the enclosing chain, no allocation
      is_qualified(name_sym)            -> classified once per symbol
      is_global_package(pkg_sym)        -> two id compares, no String clone

  src/parser/ and src/compiler/ are exempt: deciding what a name is from its
  text is their job, and doing it there instead of per execution is the point.

  If the sites only moved (a file split), move their counts to the new file's
  row by hand. See https://github.com/tokuhirom/mutsu/issues/8899.
MSG
    exit 1
fi
