use Test;

plan 11;

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
