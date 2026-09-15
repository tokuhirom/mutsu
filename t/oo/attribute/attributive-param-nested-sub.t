use v6;
use Test;

# #8452 gap 2: an attributive parameter (`$!x`) on a plain `sub` nested
# directly in a method body must bind straight to `self`'s attribute, exactly
# like the same parameter on the method itself -- not to a throwaway local
# that nobody reads. The binder writes the bound value to env/locals keyed by
# the twigil name; without the post-bind mirror into `self`'s shared
# attribute cell (`mirror_attributive_params_to_cell`), the parameter value
# was silently discarded.

plan 6;

class Box {
    has $!t = "orig";
    method set-via-nested-sub($v) {
        sub s($!t) { }
        s($v);
    }
    method get { $!t }
}

{
    my $b = Box.new;
    $b.set-via-nested-sub("new");
    is $b.get, "new", "a sub's attributive param persists to the object's attribute";
}

# Reading $!x from inside the nested sub itself must also see the bound
# value, not the pre-call attribute value.
class Box2 {
    has $!t = "orig";
    method m {
        my $seen;
        sub s($!t) { $seen = $!t }
        s("inside-value");
        $seen;
    }
}
is Box2.new.m, "inside-value",
    "reading \$!x inside the nested sub sees the bound parameter value";

# The general (non-cached) call path and the positional-light fast-dispatch
# cache path must agree: call the same sub enough times to exercise a cache
# hit, and with a fresh receiver each time.
class Box3 {
    has $!t = "orig";
    method set($v) {
        sub s($!t) { }
        s($v);
    }
    method get { $!t }
}
{
    my @b = (Box3.new, Box3.new, Box3.new);
    @b[0].set(1);
    @b[1].set(2);
    @b[2].set(3);
    is @b[0].get, 1, "fast-path repeated call 1: attributive param persists (cache miss)";
    is @b[1].get, 2, "fast-path repeated call 2: attributive param persists (cache hit)";
    is @b[2].get, 3, "fast-path repeated call 3: attributive param persists (cache hit)";
}

# A plain (non-attributive) assignment from a nested sub already worked
# before this fix; pin it as a neighbouring-behavior regression guard.
class Box4 {
    has $!t = "orig";
    method m {
        sub s() { $!t = "viaNamed" }
        s();
        $!t;
    }
}
is Box4.new.m, "viaNamed",
    "a plain assignment (no param) from a nested sub still persists to the attribute";
