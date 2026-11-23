use Test;

# The default `.gist` / `.raku` of an instance renders each public attribute's
# value. A Proxy-bound attribute runs its FETCH to do that, and the FETCH may
# write another attribute of the same instance: `AttrX::Mooish`'s lazy builder
# bumps a `++⚛$!builds` counter. That write used to wait forever for the
# attribute lock the renderer still held, so `say $obj` hung.

plan 5;

class C {
    has atomicint $!fetches = 0;
    has $.p;
    submethod TWEAK {
        my $self = self;
        $!p := Proxy.new(
            FETCH => -> $ { $self.bump; 42 },
            STORE => -> $, $ { },
        );
    }
    method bump() { ++⚛$!fetches }
    method fetches() { ⚛$!fetches }
}

my $c = C.new;
my $before = $c.fetches;
is $c.gist, 'C.new(p => 42)', '.gist renders the Proxy attribute through its FETCH';
is $c.raku, 'C.new(p => 42)', '.raku renders it too';
ok $c.fetches > $before, 'the FETCH ran, and its write to the instance landed';

# The same with the counter bumped from inside a `start` block, so the write
# comes from another thread while this one renders.
my $d = C.new;
my $p = start { $d.gist };
is await($p), 'C.new(p => 42)', 'rendering from another thread completes';

# And a lazily built value, the shape AttrX::Mooish installs.
class Lazy {
    has atomicint $!builds = 0;
    has $.value;
    submethod TWEAK {
        my $self = self;
        my $built;
        $!value := Proxy.new(
            FETCH => -> $ { $built //= $self.build },
            STORE => -> $, $v { $built = $v },
        );
    }
    method build() { ++⚛$!builds; 'built' }
}
is Lazy.new.gist, 'Lazy.new(value => "built")', 'a lazy attribute built during .gist';
