use v6;
use lib 't/lib/ResBareA/lib', 't/lib/ResBareB/lib';
use Test;

plan 2;

# Regression for the `t/http-router.rakutest` (Cro::HTTP) "resources" block
# 404s: a module with no `unit module` declaration compiles its top-level subs
# under the generic "GLOBAL" package. A nested closure literal built inside
# such a sub (e.g. `route { resources-from %?RESOURCES; ... }` in
# Cro::HTTP::Router) is (re)constructed each time the sub actually runs, not
# once at module-load time — so two bugs compounded:
#
# 1. The closure's `source_file` was read from the dynamically-scoped `?FILE`
#    env var, which only tracks the file currently being *loaded*. By the
#    time the sub is called from outside the module, `?FILE` has reverted to
#    the CALLER's file, so the nested closure was mis-attributed to it.
# 2. `%?RESOURCES` resolution fell back to a `package -> distribution`
#    hashmap keyed by the generic "GLOBAL" package name — last-loaded-module
#    wins, so loading ANY OTHER bare (no `unit module`) distribution
#    afterward clobbered the entry and broke `%?RESOURCES` for every bare
#    module's routine, not just the one whose distribution was overwritten.
use ResBareA;

my &closure = bare-a-greeting-closure();

# Load an unrelated bare distribution AFTER ResBareA. Before the fix this
# silently repointed ResBareA's routines' %?RESOURCES at ResBareB's (empty)
# resources.
use ResBareB;
bare-b-noop();

is &closure(), 'hello from ResBareA resources',
    'a nested closure built inside an already-loaded bare module resolves %?RESOURCES against its own distribution';

# Calling the exported sub itself (not a pre-built closure reference) after
# the polluting `use` must also still resolve correctly.
is bare-a-greeting-closure()(), 'hello from ResBareA resources',
    'a fresh call to the bare module sub still resolves %?RESOURCES correctly after another bare module loaded';
