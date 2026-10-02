#!/usr/bin/env bash
# Every GitHub Actions runner label is pinned to an explicit OS version
# (`ubuntu-24.04`, `macos-26`, ...), never a floating `*-latest` alias.
#
# A `-latest` alias moves to a new OS image on GitHub's schedule, not ours: the
# toolchain, glibc the release tarballs link against, apt package set and core
# count all change under an unchanged commit, and a red run then looks like a
# regression in whatever PR happened to be open. Bumping the pin is a deliberate,
# reviewable one-line change instead.
#
# Usage: scripts/check-runner-pins.sh [--self-test] [dir]
#   dir defaults to .github/workflows
set -euo pipefail

# Matches a floating label wherever a workflow can name a runner: a
# `runs-on:` value, a matrix `os:` entry, or an inline `[ubuntu-latest, ...]`.
pattern='\b(ubuntu|macos|windows)-latest\b'

scan() {
    local dir=$1
    # Comments are skipped: prose may explain why the alias is avoided.
    grep -rnE --include='*.yml' --include='*.yaml' "$pattern" "$dir" \
        | grep -vE '^[^:]+:[0-9]+:[[:space:]]*#' || true
}

if [[ ${1:-} == --self-test ]]; then
    tmp=$(mktemp -d)
    trap 'rm -rf "$tmp"' EXIT
    printf 'jobs:\n  a:\n    runs-on: ubuntu-24.04\n    # ubuntu-latest is avoided on purpose\n' >"$tmp/ok.yml"
    if [[ -n $(scan "$tmp") ]]; then
        echo 'check-runner-pins self-test: a pinned workflow was flagged' >&2
        exit 1
    fi
    for bad in '    runs-on: ubuntu-latest' '          - os: macos-latest' '    runs-on: [windows-latest]'; do
        printf 'jobs:\n  a:\n%s\n' "$bad" >"$tmp/bad.yml"
        if [[ -z $(scan "$tmp") ]]; then
            echo "check-runner-pins self-test: missed '$bad'" >&2
            exit 1
        fi
    done
    echo 'check-runner-pins self-test: ok'
    exit 0
fi

hits=$(scan "${1:-.github/workflows}")
if [[ -n $hits ]]; then
    echo 'check-runner-pins: floating runner labels found; pin an OS version (e.g. ubuntu-24.04, macos-26):' >&2
    echo "$hits" >&2
    exit 1
fi
echo 'check-runner-pins: ok'
