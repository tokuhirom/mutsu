#!/usr/bin/env bash
# Every `tests/*.rs` file is compiled into exactly one integration-test binary.
#
# Cargo.toml sets `autotests = false`: the integration tests are two binaries
# (`tests/integration.rs`, and `tests/alloc_budget.rs` for the tests that need
# a counting `#[global_allocator]`) that declare each file as a `mod`, instead
# of one target per file that each link the whole library. The cost of that is
# that a new `tests/foo.rs` nobody declares is never compiled and never run,
# silently. This guard turns that into a failure.
#
# Usage: scripts/check-integration-tests.sh [--self-test] [dir]
#   dir defaults to tests
set -euo pipefail

roots=(integration alloc_budget)

# Prints one complaint per undeclared or doubly declared file.
scan() {
    local dir=$1 f name count root
    for f in "$dir"/*.rs; do
        [[ -e $f ]] || continue
        name=$(basename "$f" .rs)
        [[ " ${roots[*]} " == *" $name "* ]] && continue
        count=0
        for root in "${roots[@]}"; do
            [[ -f $dir/$root.rs ]] || continue
            if grep -qE "^[[:space:]]*mod[[:space:]]+$name[[:space:]]*;" "$dir/$root.rs"; then
                count=$((count + 1))
            fi
        done
        if ((count == 0)); then
            echo "$f: not declared by any root (add \`mod $name;\` to $dir/integration.rs)"
        elif ((count > 1)); then
            echo "$f: declared by more than one root"
        fi
    done
}

if [[ ${1:-} == --self-test ]]; then
    tmp=$(mktemp -d)
    trap 'rm -rf "$tmp"' EXIT
    printf 'mod a;\n' >"$tmp/integration.rs"
    printf 'mod b;\n' >"$tmp/alloc_budget.rs"
    : >"$tmp/a.rs"
    : >"$tmp/b.rs"
    if [[ -n $(scan "$tmp") ]]; then
        echo 'check-integration-tests self-test: a complete layout was flagged' >&2
        exit 1
    fi
    : >"$tmp/c.rs"
    if [[ -z $(scan "$tmp") ]]; then
        echo 'check-integration-tests self-test: missed an undeclared file' >&2
        exit 1
    fi
    rm "$tmp/c.rs"
    printf 'mod a;\nmod b;\n' >"$tmp/integration.rs"
    if [[ -z $(scan "$tmp") ]]; then
        echo 'check-integration-tests self-test: missed a doubly declared file' >&2
        exit 1
    fi
    echo 'check-integration-tests self-test: ok'
    exit 0
fi

hits=$(scan "${1:-tests}")
if [[ -n $hits ]]; then
    echo 'check-integration-tests: every tests/*.rs must be a module of exactly one test root:' >&2
    echo "$hits" >&2
    exit 1
fi
echo 'check-integration-tests: ok'
