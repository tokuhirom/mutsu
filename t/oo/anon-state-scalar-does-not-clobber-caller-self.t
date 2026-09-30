use Test;

# Distilled from CSS::TagSet (CSS::Module::CSS3::Metadata's
# `our sub index { state $ //= do {...} }` called from a method): the second
# call of a routine with an anonymous `state $` replaced the calling method's
# `self` with the state value.
plan 3;

sub anon-state { state $ //= [1, 2] }

class Mod {
    has $.v = 7;
    method go {
        anon-state();
        anon-state();
        anon-state();
        self;
    }
}

my $m = Mod.new;
my $r = $m.go;
is $r.^name, 'Mod', 'self is intact after repeated calls to a sub with an anonymous state var';
is $r.v, 7, 'attribute accessors still work on it';
is-deeply anon-state(), [1, 2], 'the anonymous state value itself is stable';
