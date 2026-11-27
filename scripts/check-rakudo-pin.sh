#!/usr/bin/env bash
# One rakudo release, pinned in `.rakudo-version`, for everything that installs
# rakudo: the agents' oracle (install-raku.sh, .claude/hooks/session-start.sh),
# the bench CI's ratio baseline (bench.yml) and the ecosystem sweep. They read
# the pin through `install-raku.sh`; none names a release of its own.
#
# Before the pin, bench.yml hard-coded a rakudo.org archive while the oracle
# installed "the newest prebuilt", so the oracle and the bench baseline drifted
# apart (bench measured 2026.07 for months after the oracle had moved on).
#
# Fails when `.rakudo-version` is not one release such as `2026.11`, or when a
# non-comment line in a workflow, hook, skill or script names a rakudo.org
# archive (`rakudo-moar-2026.11-...`) or passes install-raku.sh a literal
# `--version`.
#
# Usage: scripts/check-rakudo-pin.sh [--self-test] [root]
set -euo pipefail

patterns=(
    'rakudo-moar-[0-9]{4}\.[0-9]{2}'
    'install-raku\.sh.*--version[ =]+[0-9]{4}\.'
)

scan() {
    local root=$1 p dirs=()
    for d in .github .claude .agents scripts; do
        [ -d "$root/$d" ] && dirs+=("$root/$d")
    done
    [ ${#dirs[@]} -gt 0 ] || return 0
    for p in "${patterns[@]}"; do
        # Comments are skipped: prose may name an archive as an example.
        grep -rnE --exclude='check-rakudo-pin.sh' "$p" "${dirs[@]}" \
            | grep -vE '^[^:]+:[0-9]+:[[:space:]]*#' || true
    done
}

valid_pin() {
    head -n1 "$1" | tr -d '[:space:]' | grep -Eqx '[0-9]{4}\.[0-9]{2}(\.[0-9]+)?'
}

if [[ ${1:-} == --self-test ]]; then
    tmp=$(mktemp -d)
    trap 'rm -rf "$tmp"' EXIT
    mkdir -p "$tmp/.github/workflows"
    printf 'steps:\n  # e.g. rakudo-moar-2026.11-01-linux-x86_64-gcc.tar.gz\n  - run: install-raku.sh --version "$V"\n' \
        >"$tmp/.github/workflows/ok.yml"
    if [[ -n $(scan "$tmp") ]]; then
        echo 'check-rakudo-pin self-test: a workflow reading the pin was flagged' >&2
        exit 1
    fi
    for bad in '  RAKUDO_ARCHIVE: "rakudo-moar-2026.11-01-linux-x86_64-gcc.tar.gz"' \
               '  - run: .agents/skills/install-raku/install-raku.sh --version 2026.07'; do
        printf 'steps:\n%s\n' "$bad" >"$tmp/.github/workflows/bad.yml"
        if [[ -z $(scan "$tmp") ]]; then
            echo "check-rakudo-pin self-test: missed '$bad'" >&2
            exit 1
        fi
    done
    for v in 2026.11 2026.11.1; do
        printf '%s\n' "$v" >"$tmp/pin"
        valid_pin "$tmp/pin" || { echo "check-rakudo-pin self-test: rejected $v" >&2; exit 1; }
    done
    for v in '' latest 2026.7 'v2026.11'; do
        printf '%s\n' "$v" >"$tmp/pin"
        if valid_pin "$tmp/pin"; then
            echo "check-rakudo-pin self-test: accepted '$v'" >&2
            exit 1
        fi
    done
    echo 'check-rakudo-pin self-test: ok'
    exit 0
fi

root=${1:-.}
if [ ! -f "$root/.rakudo-version" ] || ! valid_pin "$root/.rakudo-version"; then
    echo "check-rakudo-pin: $root/.rakudo-version must hold one rakudo release such as 2026.11" >&2
    exit 1
fi
found=$(scan "$root")
if [[ -n $found ]]; then
    echo "check-rakudo-pin: these lines pin a rakudo release of their own; read .rakudo-version" >&2
    echo "through .agents/skills/install-raku/install-raku.sh (--print-pin, or no --version) instead:" >&2
    echo "$found" >&2
    exit 1
fi
echo "check-rakudo-pin: ok ($(head -n1 "$root/.rakudo-version" | tr -d '[:space:]'))"
