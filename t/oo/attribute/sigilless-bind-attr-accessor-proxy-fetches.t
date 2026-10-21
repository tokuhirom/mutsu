use Test;

# `$.x` is `self.x` itemized, so `my \e = $.x` over a Proxy-returning `x`
# reads the container at the bind: the FETCH's side effects are visible to the
# next statement (PDF::COS::Stream.content relies on that to set /Length).
plan 3;

class C {
    has $.len;
    has $!v = 'abcde';
    method v is rw {
        Proxy.new:
            FETCH => -> $ { $!len = $!v.chars; $!v },
            STORE => -> $, $x { $!v = $x };
    }
    method probe {
        my \e = $.v;
        $!len;
    }
    method value {
        my \e = $.v;
        e;
    }
}

my $c = C.new;
is $c.probe, 5, 'the FETCH ran when the name was bound';
is $c.value, 'abcde', 'the bound name holds the fetched value';
$c.v = 'xy';
is $c.probe, 2, 'after a store, the bind fetches the new value';
