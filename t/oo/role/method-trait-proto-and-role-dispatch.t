use Test;

# Distribution: Method::Also (t/01-basic.rakutest). A method-level
# `trait_mod:<is>(Method ...)` handler must run for `proto method`
# declarations (with the trait argument), for role methods and role proto
# methods (once, at declaration, with $*PACKAGE bound to the role), and for
# class methods (with $*PACKAGE bound to the class).

my @seen;
multi sub trait_mod:<is>(Method:D \meth, :$also!) {
    @seen.push: ($*PACKAGE.^name, meth.name, $also.join(','));
}

class C {
    proto method p(|) is also<q r> {*}
    multi method p(Int) { 1 }
}
role R {
    proto method rp(|) is also<rq> {*}
    multi method rp(Str) { 2 }
    method rm() is also<rn> { 3 }
}
class D does R { }

is-deeply @seen.sort(*[1]).list,
    (("C", "p", "q,r"), ("R", "rm", "rn"), ("R", "rp", "rq")),
    'traits reached trait_mod:<is> once per declaration, with args and $*PACKAGE';
is D.rp("x"), 2, 'role proto method still dispatches';
is C.p(1), 1, 'class proto method still dispatches';

done-testing;
