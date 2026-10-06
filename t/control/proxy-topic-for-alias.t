use v6;
use Test;

# From the Hash::MutableKeys distribution: `$_` in a `for` is an rw alias of a
# Proxy element, so assigning to it calls the element's STORE.

plan 6;

my @stored;
sub mk($tag) {
    Proxy.new(FETCH => { $tag }, STORE => -> $, $v { @stored.push("$tag=$v"); $v });
}

$_ = "x" for (1, 2).map: -> $k { Proxy.new(FETCH => { $k }, STORE => -> $, $v { @stored.push("$k=$v"); $v }) };
is @stored.join(','), '1=x,2=x', 'statement-modifier for assigns through each Proxy';

@stored = ();
for (1, 2).map: -> $k { Proxy.new(FETCH => { $k }, STORE => -> $, $v { @stored.push("$k=$v"); $v }) } {
    $_ = "y";
}
is @stored.join(','), '1=y,2=y', 'block for assigns through each Proxy';

@stored = ();
my class P is Proxy is Cool { }
$_ .= uc for (<a b>).map: -> $k { P.new(FETCH => { $k }, STORE => -> $, $v { @stored.push("$k=$v"); $v }) };
is @stored.join(','), 'a=A,b=B', '.= on the topic stores the transformed FETCH';

# A named parameter binds the FETCHed value (read-only), not the Proxy.
my @seen;
for (1, 2).map: -> $k { Proxy.new(FETCH => { $k * 10 }, STORE => -> $, $v { }) } -> $v {
    @seen.push($v);
}
is @seen.join(','), '10,20', 'a named parameter sees the FETCHed values';

# Role-is-Hash tie whose keys yield Proxies (Hash::MutableKeys shape).
role MK is Hash {
    my class KP is Proxy is Cool { }
    method keys() {
        callsame.sort.map: -> $key {
            KP.new(FETCH => { $key }, STORE => -> $, $new {
                self.ASSIGN-KEY($new, self.DELETE-KEY($key));
                $new
            })
        }
    }
}
my %h is MK = a => 1, b => 2;
$_ .= uc for %h.keys;
is %h.gist, '{A => 1, B => 2}', 'for %tied.keys assigns through the key Proxies';

my %i is MK = a => 1;
$_ = "z" for %i.keys;
is %i.gist, '{z => 1}', 'plain assignment through a key Proxy renames the key';
