use Test;

# A Proxy whose FETCH ends in a call to an `is rw` routine answers that
# routine's container. Reading the Proxy in value context must hand back what
# is IN the container: a chained method call on it used to dispatch on the
# container and find none of the value's methods ("No such method 'foo' for
# invocant of type 'St2'"). Found through Tinky's `state` Proxy, whose FETCH is
# `$SELF!state` over an `is rw` private method.

plan 5;

class St { has $.foo }
class Obj {
    has $.state;
    method !state() is rw { $!state }
    method pubrw() is rw { $!state }
    method via-private(Obj:D $SELF:) is rw {
        Proxy.new(FETCH => method () { $SELF!state }, STORE => method ($v) { $SELF!state = $v })
    }
    method via-public() is rw {
        my $s = self;
        Proxy.new(FETCH => method () { $s.pubrw }, STORE => method ($v) { })
    }
}

my $o = Obj.new(state => St.new(foo => 'f'));
is $o.via-private.foo, 'f', 'chained call through a private is-rw FETCH';
is $o.via-public.foo, 'f', 'chained call through a public is-rw FETCH';
is $o.via-private.^name, 'St', 'the fetched value reports its own type';
$o.via-private = St.new(foo => 'g');
is $o.via-private.foo, 'g', 'STORE through the same Proxy still writes';
is $o.state.foo, 'g', 'and the attribute sees it';
