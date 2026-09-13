#!/usr/bin/env bash
# Ban on hand-built `__mutsu_*` metadata keys (issue #8087).
#
# `format!("__mutsu_<ns>::{name}")` followed by an `Env` probe has been the
# profiling finding in five separate perf campaigns (#7571, #7766, the two
# bench-ctor rounds, and #8069's element store at 22 interns and 8 heap
# allocations per `@a[$i] = $v`). Each was fixed by memoizing the one key that
# profile happened to walk through, and the pattern grew back, because nothing
# stopped the next site being written.
#
# `MetaNs` (src/runtime/meta_ns.rs) is the memoizing constructor that builds
# every one of these keys, and as of stage 3 of #8087 it builds ALL of them:
# this script started life as a ratchet over a 276-site baseline, which stage 2
# took to 174 and stage 3 to zero. With the debt gone the baseline file went
# with it, and what is left is the simpler rule -- there is no hand-built form
# any more, so a new one is a build failure, not a number to compare.
#
#   scripts/check-magic-keys.sh      # `make check-magic-keys`, a `make test`
#                                    # prerequisite and a CI step
#
# The endgame is that these keys stop existing altogether, because the metadata
# moves onto the binding's own descriptor (#8069 §4.1) or off the per-frame env
# (#7817 / ADR-0084). This gate is what keeps the ground from being lost while
# that happens.
set -euo pipefail

cd "$(dirname "$0")/.."

# One hand-built key site = a format-string LITERAL that opens a `__mutsu_`
# namespace and then interpolates: `"__mutsu_<ns>::{"` or `"__mutsu_<ns>__{"`.
#
# Matching the literal rather than `format!("__mutsu_` is deliberate, and was
# the fix for a hole this gate shipped with: a `format!` whose literal sits on
# the NEXT line -- which is how rustfmt writes any call that does not fit on one
# -- was invisible to it. Eight real sites were hiding behind that,
# `__mutsu_callable_id::` among them, in the very namespace the previous stage
# had just declared clear.
#
# The `::` / `__` in the pattern is what separates a key from a compiler
# temporary, and the distinction is a convention the codebase already keeps
# without exception:
#
#   `__mutsu_<ns>::<name>`  an ENV key, derived from a binding's name
#   `__mutsu_<ns>__<name>`  a MIXIN-REGISTRY key, derived from a role/attr name
#   `__mutsu_<kind>_<n>`    a gensym: one fresh unique local name per compile
#                           site, with a counter, not a name, after the `_`
#
# Only the first two are what this issue is about. They are *probed* -- built
# from a name that something else also has, over and over at runtime, which is
# why memoizing them pays and why a spelling mismatch between the writer and
# the reader silently loses the metadata. A gensym is built once at compile
# time and handed straight to `alloc_local`; nothing ever looks it up by
# rebuilding it, so there is nothing to memoize and nothing to get out of step.
#
# So: a new namespace key must go through `MetaNs`, and a new temporary is free
# to keep its `format!` -- but it must keep the single-underscore shape, or this
# gate will (correctly) start counting it.
#
# src/runtime/meta_ns.rs is exempt -- it is the constructor those sites are
# supposed to be using, and its own `format!` is the one that is allowed.
# Comment lines are skipped: prose that quotes a key is not a call site.
sites=$(
    grep -rnE '"__mutsu_[A-Za-z0-9_]*(::|__)\{' src/ --include='*.rs' \
        | grep -v '^src/runtime/meta_ns.rs:' \
        | grep -vE '^[^:]*:[0-9]+: *(//|\*)' \
        || true
)

if [ -n "$sites" ]; then
    echo "check-magic-keys: hand-built __mutsu_* metadata key(s):" >&2
    echo "$sites" | sed 's/^/  /' >&2
    cat >&2 <<'MSG'

  Build these with MetaNs instead (src/runtime/meta_ns.rs):

      MetaNs::Type.key(sym)               -> Symbol, memoized; probe with
                                             Env::get_sym / contains_key_sym
      MetaNs::Role.str_key_for_str(name)  -> &'static str, for the String-keyed
                                             mixin registry
      MetaNs::CallableId.key_pair(a, b)   -> a two-part key

  Add a variant for a namespace that has none yet, and pin its spelling in
  meta_ns.rs's `every_namespace_spells_its_key_exactly_as_the_format_sites_did`.

  A WRITE must go through Env::insert_sym_noting, not insert_sym: insert_sym
  skips note_env_key, which leaves the reader's `*_possible()` fast-path probe
  switched off for the rest of the process -- no error, just metadata that is
  never found again.

  See https://github.com/tokuhirom/mutsu/issues/8087.
MSG
    count=$(echo "$sites" | wc -l | tr -d ' ')
    echo "check-magic-keys: FAILED ($count site(s))" >&2
    exit 1
fi

echo "check-magic-keys: ok (no hand-built __mutsu_* metadata keys)"
