use v6.d;
use TagExp;
use TagExp :internals;

# Same as UsesInternals, but does a plain `use` first and then a tagged
# re-import. The default `use` strips the :internals export; the tagged
# re-import must restore it so the module's own methods resolve it.
class UsesInternalsDouble is export {
    method run($x) {
        return internal($x);
    }
    method plain() {
        return always();
    }
}
