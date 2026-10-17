use Test;

# Found while running the Math::Matrix suite: AttrX::Lazy's attribute trait
# mixes a `compose` hook into the class HOW; the hook ran twice, so its
# "method already exists" check tripped over the accessor it had just added.

plan 3;

my @log;
my role CountingHOW {
    method compose(Mu \type) {
        @log.push: 1;
        for type.^attributes.grep(*.name eq '$!z') {
            die "accessor already exists" if "z" ~~ type.^method_table;
            type.^add_method("z", method { 42 });
        }
        callsame;
    }
}
multi trait_mod:<is>(Attribute $a, :$counted!) { $a.package.HOW does CountingHOW }

class C {
    has $!z is counted;
    method Bool { True }
}

is @log.elems, 1, 'the HOW compose hook ran once';
is C.new.z, 42, 'the accessor the hook added works';
ok C.new.Bool, 'other methods are unaffected';
