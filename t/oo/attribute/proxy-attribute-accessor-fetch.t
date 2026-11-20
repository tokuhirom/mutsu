use Test;

# A generated accessor over an attribute bound to a Proxy FETCHes it on
# every read (AttrX::Mooish's lazy attribute shape, #18267). The read is
# answered by the accessor fast path; an `is rw` accessor still hands back
# the Proxy, and a dying FETCH raises its own exception exactly once.

plan 9;

class P {
    has $.v;
    has $.w is rw;
    has $!n = 0;
    method setup {
        $!v := Proxy.new(FETCH => -> $ { $!n++; [1, 2, $!n] }, STORE => -> $, $ { die "ro" });
        $!w := Proxy.new(FETCH => -> $ { "w" }, STORE => -> $, $ { });
        self
    }
    method n { $!n }
}

my $p = P.new.setup;
is-deeply $p.v, [1, 2, 1], 'the first read FETCHes';
is-deeply $p.v, [1, 2, 2], 'every read FETCHes again';
is-deeply (^3).map({ $p.v[2] }).List, (3, 4, 5), 'repeated reads in a loop';
is $p.n, 5, 'FETCH ran once per read';
is $p.w, 'w', 'an is rw accessor reads through its Proxy';

class Q {
    has $.v;
    has $!fetches = 0;
    method setup {
        $!v := Proxy.new(FETCH => -> $ { $!fetches++; die "boom" }, STORE => -> $, $ { });
        self
    }
    method fetches { $!fetches }
}
my $q = Q.new.setup;
my @caught;
for ^2 { try { $q.v; CATCH { default { @caught.push: .message } } } }
is-deeply @caught, ['boom', 'boom'], 'a dying FETCH raises through the accessor';
is $q.fetches, 2, 'a dying FETCH runs once per read';

class Lazy {
    has $.x;
    method setup { my $cache; $!x := Proxy.new(FETCH => -> $ { $cache //= 42 }, STORE => -> $, $v { $cache = $v }); self }
}
my $l = Lazy.new.setup;
is $l.x, 42, 'a lazily built value';
is $l.x, 42, 'and its cached reread';
