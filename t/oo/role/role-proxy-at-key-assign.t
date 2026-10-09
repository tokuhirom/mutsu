use Test;

# From Hash::Ordered / Hash::Agnostic: `self.AT-KEY($k) = v` inside a role mixed
# into a Hash (`my %h is R`) assigns through the Proxy that AT-KEY returns.

plan 3;

role R {
    has %!h;
    method AT-KEY(::?ROLE:D: \key) is raw {
        Proxy.new(
            FETCH => { %!h{key} // Nil },
            STORE => -> $, \value { %!h{key} = value },
        )
    }
    method ASSIGN-KEY(::?ROLE:D: $key, Mu \value) is raw {
        self.AT-KEY($key) = value;
    }
    method STORE(::?ROLE:D: *@v) { self.ASSIGN-KEY(.key, .value) for @v; self }
    method get($k) { %!h{$k} }
}

my %x is R = a => 1, b => 2;
is %x.get("a"), 1, 'STORE assigns through the AT-KEY Proxy';
is %x.get("b"), 2, 'second pair too';
%x.ASSIGN-KEY("c", 3);
is %x.get("c"), 3, 'direct ASSIGN-KEY works';
