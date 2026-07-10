use v6.d;
use TagExp :internals;

# A nested module imports a tag-gated sub from another module. Its own methods
# must be able to resolve that imported sub, even though the outer program that
# `use`s this module never requested :internals.
class UsesInternals is export {
    method run($x) {
        return internal($x);
    }
}
