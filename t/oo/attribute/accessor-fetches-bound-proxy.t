use Test;
use nqp;

plan 8;

# A generated, non-rw accessor hands back a decontainerized value, so an
# attribute bound to a Proxy is read through it: the FETCH runs at the call,
# even in statement position (AttrX::Mooish's lazy `has %.h is mooish(:lazy)`).
my class MyP is Proxy { }

sub dies-fetching(&call) {
    try { call(); CATCH { default { return True } } }
    False
}

class Plain {
    has $.s;
    has %.h;
    has @.l;
    method bind-s { $!s := Proxy.new(FETCH => method () { die "F" }, STORE => method ($) { }); Nil }
    method bind-h { nqp::bindattr(self, Plain, '%!h', Proxy.new(FETCH => method () { die "F" }, STORE => method ($) { })); Nil }
    method bind-l { nqp::bindattr(self, Plain, '@!l', Proxy.new(FETCH => method () { die "F" }, STORE => method ($) { })); Nil }
}

class Sub {
    has %.h;
    method bind-h { nqp::bindattr(self, Sub, '%!h', MyP.new(FETCH => method () { die "F" }, STORE => method ($) { })); Nil }
}

my $s = Plain.new; $s.bind-s;
ok dies-fetching({ $s.s; }), "a scalar attribute's accessor FETCHes in statement position";
my $h = Plain.new; $h.bind-h;
ok dies-fetching({ $h.h; }), "a hash attribute's accessor FETCHes in statement position";
my $l = Plain.new; $l.bind-l;
ok dies-fetching({ $l.l; }), "an array attribute's accessor FETCHes in statement position";
my $sub = Sub.new; $sub.bind-h;
ok dies-fetching({ $sub.h; }), "so does one bound to a Proxy subclass";

ok dies-fetching({ my $x = $h.h; }), "an assignment from the accessor FETCHes";
ok dies-fetching({ my $x = $sub.h; }), "also through a Proxy subclass";

my $unbound = Plain.new;
nok dies-fetching({ $unbound.h; }), "an unbound attribute's accessor reads normally";
nok dies-fetching({ $unbound.s; }), "also a scalar one";
