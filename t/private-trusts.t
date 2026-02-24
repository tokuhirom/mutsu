use Test;

plan 4;

class Trustee { ... }
class Truster {
    trusts Trustee;
    has $.x;
    method !get-x-priv {
        $!x;
    }
}

class ChildTruster is Truster { }

class Trustee {
    method x($truster) {
        $truster!Truster::get-x-priv();
    }
}

throws-like 'Truster.new()!Truster::get-x-priv', X::Method::Private::Permission,
    'private method access without trust fails';
is Trustee.x(Truster.new(x => 5)), 5, 'trusted class can call private method';
is Trustee.x(ChildTruster.new(x => 5)), 5, 'trust works with truster subclasses';
throws-like q[class ChildTrustee is Trustee { method x($t) { $t!Truster::get-x-priv() } }],
    X::Method::Private::Permission,
    'trust relation does not apply to trustee subclasses';
