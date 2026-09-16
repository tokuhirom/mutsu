use v6;
use Test;

# Config::TOML (raku-community-modules) exports a plain `sub to-toml(Associative:D
# $container, ...)`; its own dependency, `Config::TOML::Dumper`, separately
# declares an unrelated, unexported `multi sub to-toml(Str:D $s)` / `multi sub
# to-toml(Int:D $i)` it uses internally to render scalar values. The two wound
# up sharing one registry base-name bucket once mutsu's per-compunit lexical
# scoping collapsed them, so `Dumper`'s own bare `to-toml(...)` calls picked
# the OUTER exported plain sub over its own multi candidates every time --
# https://github.com/tokuhirom/mutsu/issues/7539.
#
# Reduced here as two library modules with the identical shape: `Inner`
# declares only the multi candidates (never exported), `Outer` `use`s `Inner`
# and separately exports a same-named plain sub. `Inner` never `use`s `Outer`,
# so nothing in `Inner`'s own compunit should ever see `Outer`'s routine.

plan 5;

use lib 't/lib/CrossCompunitMultiVsPlainSub';
use Outer;

is to-toml({a => "x", b => 1}), 'a="x",b=1',
    "the outer plain sub renders through Inner's own multi candidates, not itself";

is to-toml({one => 1}), 'one=1', "single Int value";
is to-toml({s => "solo"}), 's="solo"', "single Str value";

# `.&to-toml` inside a `.map` closure, alternating argument types more than
# once at the SAME call site: the VM's per-name "has multi candidates" light-
# call-cache probe must not answer a later, correctly-multi call from inside
# `Inner` with a stale negative computed from an earlier, unrelated package's
# probe -- otherwise the type-blind light-call cache latches onto whichever
# candidate resolved first and reuses it for every later value regardless of
# its own type.
is to-toml-list([1, "x", 2]), '1,"x",2',
    "a .map closure alternating argument types dispatches each call correctly";
is to-toml-list(["y", 3, "z"]), '"y",3,"z"',
    "repeating with the types in a different order still dispatches correctly";
