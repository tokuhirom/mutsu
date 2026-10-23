use v6;
use Test;
use nqp;

plan 2;

# A read-only accessor over an attribute bound to a Proxy hands back the
# FETCHed value, so assigning to it names that value, not the Proxy.
class C {
    has $.baz;
    submethod TWEAK {
        my $p := Proxy.new(FETCH => -> $ { "v" }, STORE => -> $, $v { });
        nqp::bindattr(self, C, '$!baz', $p);
    }
}
my $c = C.new;
throws-like { $c.baz = "x" }, X::Assignment::RO, message => 'Cannot modify an immutable Str (v)',
    'assignment to read-only accessor over a Proxy attribute';
is $c.baz, "v", 'accessor reads through the Proxy';

done-testing;
