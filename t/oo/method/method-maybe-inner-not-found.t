use Test;

plan 22;

my $runs = 0;
class F {
    method inner { $runs++; self.no-such-thing }
}

my $f = F.new;
is $f.?absent, Nil, '.? answers Nil when its own lookup misses';
throws-like { $f.?inner }, X::Method::NotFound,
    message => /'no-such-thing'/,
    '.? propagates a missing method called from a found method';
is $runs, 1, 'the found method ran once';

throws-like { F.new.?inner }, X::Method::NotFound,
    message => /'no-such-thing'/,
    'the temporary receiver path propagates the inner miss';
is $runs, 2, 'the temporary receiver method ran once';

throws-like { (F.new, F.new)».?inner }, X::Method::NotFound,
    message => /'no-such-thing'/,
    'the hyper path propagates the inner miss';
is $runs, 3, 'hyper dispatch stops after the first failing element';

class Missing { }
class Outer { method probe { Missing.new.probe } }
throws-like { Outer.new.?probe }, X::Method::NotFound,
    message => /'Missing'/,
    'a same-named miss on another receiver propagates';

role R { }
is (F.new but R).?absent, Nil, 'a composed receiver still answers Nil on a miss';

class G { method FALLBACK($name) { "fallback:$name" } }
is G.new.?absent, 'fallback:absent', '.? still dispatches through FALLBACK';
is $f.?absent, Nil, 'a later missing lookup remains optional';

# The receiver's own miss is recognised however dispatch spells its type: a
# List literal seen as an Array, a `.VAR` descriptor, an Int read through an
# array element, a mixin over a scalar value.
is (1, 2).?absent, Nil, 'a List literal answers Nil on its own miss';
my $x = 5;
is $x.VAR.?is-building, Nil, 'a .VAR descriptor answers Nil on its own miss';
my @a = 1, 2;
is @a[0].VAR.?absent, Nil, 'an element .VAR answers Nil too';
is @a[0].?absent, Nil, 'an Int in an array element answers Nil';
is (0.25 but R).?absent, Nil, 'a mixin over a Rat answers Nil';
is "s".?absent, Nil, 'a Str answers Nil';
throws-like { my $y = F.new; $y.VAR; $y.?inner }, X::Method::NotFound,
    message => /'no-such-thing'/,
    'an inner miss through a scalar container still propagates';

# A user class inheriting from a core type misses under the parent's name.
class SubArray is Array { method probe { self.?absent } }
is SubArray.new.probe, Nil, 'an Array subclass answers Nil from inside its own method';
my $sa = SubArray.new;
is $sa.?absent, Nil, 'an Array subclass answers Nil on its own miss';
class SubHash is Hash { method probe { self.?absent } }
is SubHash.new.probe, Nil, 'a Hash subclass answers Nil';
class Inner is Array { method probe { F.new.?inner } }
throws-like { Inner.new.probe }, X::Method::NotFound, message => /'no-such-thing'/,
    'an inner miss inside a subclass method still propagates';
