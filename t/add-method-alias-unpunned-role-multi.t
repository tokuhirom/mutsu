use v6;
use Test;

# ADR-0019 F4a: `^add_method` aliasing a `^find_method` carrier clones the
# whole multi candidate family (t/can-multi-dispatcher.t already covers the
# class-owned case). When the carrier comes from a role that is never
# `.new`-punned and never `does`-composed anywhere -- `R.^find_method('m')`
# with `R` a bare role type object -- the source class name tagged on the
# carrier names the role itself, which has no row in the canonical method
# table. Without the un-punned-role fallback, only the carrier's own single
# candidate survived the clone, silently dropping every other multi
# candidate.

plan 2;

role R {
    multi method m (Int $x) { "int $x" }
    multi method m (Str $x) { "str $x" }
}
class C { }
BEGIN {
    C.^add_method('n', R.^find_method('m'));
}

is C.new.n(5), 'int 5', 'aliasing an unpunned role multi keeps the Int candidate';
is C.new.n("a"), 'str a', 'aliasing an unpunned role multi keeps the Str candidate too';
