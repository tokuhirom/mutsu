# A compilation unit whose package-less top-level `sub secret-helper` is NOT
# exported. Raku scopes such a routine lexically to this file; it must stay
# reachable from this file's own routines (an exported sub, a method of a class
# declared here, a block inside one) and be invisible to whoever loads us.
# See t/module-private-sub-does-not-leak.t.

sub secret-helper($n) { $n * 3 }

# A private helper whose name the loading scope does NOT also declare, so the
# loader sees no routine of this name at all.
sub hidden-only($n) { $n * 7 }

sub visible-hidden-only($n) is export { hidden-only($n) }

sub visible-helper($n) is export { secret-helper($n) }

class PrivateSubBox is export {
    has $.n;
    method tripled() { secret-helper($.n) }
}

sub via-block() is export { (1, 2).map({ secret-helper($_) }).join(",") }
