#!/usr/bin/env bash
# The ADR directory's guard and its index.
#
# Two things about `docs/adr/` used to be coordinated by hand, and both broke
# under parallel PRs:
#
#   numbering  Each author took "highest number on my main + 1", so two
#              concurrent PRs took the same number. It happened at least six
#              times (0054/0055, 0111/0112, 0112/0113, 0133, 0136, ...), once
#              only noticed after both had merged. A new ADR is therefore
#              numbered by the GitHub issue that carries its decision: GitHub
#              hands out that number, so it cannot collide. Files numbered the
#              old way (four digits, zero-padded, up to LEGACY_MAX) are frozen.
#   the index  docs/adr/README.md kept a hand-written table of every ADR and
#              its status. Every new ADR appended a row to its last line and
#              every landed slice edited a row, so sibling PRs conflicted on
#              it constantly -- and the row's status drifted from the ADR's own
#              Status line, which is the one that counts. The table is gone;
#              `index` builds it from the files on demand.
#
#   scripts/adr.sh check        # `make check-adr`; also run by CI's always-on
#                               # `changes` job, so docs-only PRs are covered
#   scripts/adr.sh index        # `make adr-index`: Markdown table on stdout
#   scripts/adr.sh --self-test  # proves `check` still rejects what it should
#
# ADR_DIR overrides the directory (the self-test uses it).
set -euo pipefail

cd "$(dirname "$0")/.."

ADR_DIR=${ADR_DIR:-docs/adr}
# The last number handed out under the old sequential scheme; the final two
# landed (0137 via #10475, 0138) as the scheme changed.
LEGACY_MAX=138

# The ADR files, one per line, sorted by number.
adr_files() {
    find "$ADR_DIR" -maxdepth 1 -type f -name '*.md' ! -name README.md -printf '%f\n' |
        sort -t- -k1,1n
}

# The title from the H1, without its `ADR-N:` / `ADR-N —` / `N.` prefix.
adr_title() {
    head -n1 "$1" | sed -E 's/^#[[:space:]]+//; s/^(ADR-)?[0-9]+[[:space:]]*(:|\.|—|-)[[:space:]]*//'
}

# The Status entry with its indented continuation lines joined, without its
# label and bold markup.
adr_status() {
    awk '
        found && /^[[:space:]]+[^[:space:]]/ { sub(/^[[:space:]]+/, " "); s = s $0; next }
        found { exit }
        /^(- )?(\*\*)?Status(\*\*)?:/ { found = 1; s = $0 }
        END { print s }
    ' "$1" |
        sed -E 's/^(- )?(\*\*)?Status(\*\*)?:[[:space:]]*//; s/\*\*//g; s/\|/\\|/g'
}

check() {
    local failures=0 f num n h
    local -A seen=()
    while IFS= read -r f; do
        if ! [[ $f =~ ^([0-9]+)-[a-z0-9]+(-[a-z0-9]+)*\.md$ ]]; then
            echo "not ok - $ADR_DIR/$f: name must be <number>-<kebab-title>.md" >&2
            failures=$((failures + 1))
            continue
        fi
        num=${BASH_REMATCH[1]}
        n=$((10#$num))
        if [[ -n ${seen[$n]:-} ]]; then
            echo "not ok - $ADR_DIR/$f: number $n is already taken by ${seen[$n]}" >&2
            failures=$((failures + 1))
        fi
        seen[$n]=$f
        if [[ ${#num} -eq 4 && $num == 0* ]]; then
            if ((n > LEGACY_MAX)); then
                echo "not ok - $ADR_DIR/$f: sequential numbers stopped at $(printf %04d $LEGACY_MAX);" \
                    "number a new ADR by its GitHub issue (e.g. 10226-$(echo "${f#*-}"))" >&2
                failures=$((failures + 1))
            fi
            continue
        fi
        # An issue-numbered ADR: no zero padding, past the legacy range, and
        # the strict header shape the index reads.
        if [[ $num == 0* ]] || ((n <= LEGACY_MAX)); then
            echo "not ok - $ADR_DIR/$f: an issue number has no zero padding and is above $LEGACY_MAX" >&2
            failures=$((failures + 1))
        fi
        h=$(head -n1 "$ADR_DIR/$f")
        if ! [[ $h =~ ^"# ADR-$n: ". ]]; then
            echo "not ok - $ADR_DIR/$f: first line must be '# ADR-$n: <title>'" >&2
            failures=$((failures + 1))
        fi
        if ! grep -qE '^- \*\*Status\*\*: .' "$ADR_DIR/$f"; then
            echo "not ok - $ADR_DIR/$f: needs a '- **Status**: ...' line" >&2
            failures=$((failures + 1))
        fi
    done < <(adr_files)
    if ((failures)); then
        echo "adr check: $failures problem(s); conventions in docs/adr/README.md" >&2
        return 1
    fi
    echo "adr check: ${#seen[@]} ADRs ok"
}

index() {
    local f n
    echo '| # | Title | Status |'
    echo '|---|---|---|'
    while IFS= read -r f; do
        n=${f%%-*}
        printf '| [%s](%s) | %s | %s |\n' "$n" "$ADR_DIR/$f" \
            "$(adr_title "$ADR_DIR/$f" | sed 's/|/\\|/g')" "$(adr_status "$ADR_DIR/$f")"
    done < <(adr_files)
}

self_test() {
    local dir out failures=0
    dir=$(mktemp -d)
    trap 'rm -rf "$dir"' RETURN

    expect() { # <ok|fail> <description> <pattern-in-output>
        local want=$1 what=$2 pat=$3 rc=0
        out=$(ADR_DIR=$dir "$0" check 2>&1) || rc=$?
        if [[ $want == ok && $rc -ne 0 ]] || [[ $want == fail && $rc -eq 0 ]] ||
            ! grep -qF -- "$pat" <<<"$out"; then
            echo "not ok - self-test: $what" >&2
            echo "$out" | sed 's/^/    /' >&2
            failures=$((failures + 1))
        fi
        rm -f "$dir"/*.md
    }
    adr() { printf '%s\n\n- **Status**: Accepted\n' "$2" >"$dir/$1"; }

    adr 0001-legacy.md '# 0001. Old heading shape is tolerated'
    adr 10226-new-one.md '# ADR-10226: A new one'
    expect ok "legacy and issue-numbered ADRs pass" "2 ADRs ok"

    adr 0139-too-late.md '# ADR-0139: Too late'
    expect fail "a new sequential number is rejected" "number a new ADR by its GitHub issue"

    adr 0012-a.md '# ADR-0012: A'
    adr 0012-b.md '# ADR-0012: B'
    expect fail "a duplicate number is rejected" "already taken"

    adr 10226-x.md '# ADR-10225: Wrong number'
    expect fail "an H1 that disagrees with the file name is rejected" "first line must be"

    printf '# ADR-10226: No status\n' >"$dir/10226-x.md"
    expect fail "a missing Status line is rejected" "Status"

    adr 010226-x.md '# ADR-10226: Padded'
    expect fail "a zero-padded issue number is rejected" "no zero padding"

    adr 10226-Bad_Name.md '# ADR-10226: Bad'
    expect fail "a non-kebab file name is rejected" "kebab"

    adr 10226-pipe.md '# ADR-10226: A | B'
    out=$(ADR_DIR=$dir "$0" index)
    grep -qF '| [10226](' <<<"$out" && grep -qF 'A \| B | Accepted |' <<<"$out" || {
        echo "not ok - self-test: index row" >&2
        echo "$out" | sed 's/^/    /' >&2
        failures=$((failures + 1))
    }

    if ((failures)); then
        echo "adr self-test: $failures failure(s)" >&2
        return 1
    fi
    echo "adr self-test: ok"
}

case "${1:-}" in
check) check ;;
index) index ;;
--self-test) self_test ;;
*)
    echo "usage: $0 check | index | --self-test" >&2
    exit 2
    ;;
esac
