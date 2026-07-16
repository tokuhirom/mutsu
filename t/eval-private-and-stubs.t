# EVAL inside a method: (1) a preceding forward stub that is defined LATER in
# the same compilation unit must not make EVAL fail with X::Package::Stubbed,
# and (2) a typo'd `self!private()` call must error at compile time even when a
# preceding `return` would short-circuit it at runtime (raku resolves private
# method dispatch at compile time).
use Test;
plan 5;

class Base {...}   # forward stub, defined below

class Widget {
    method run() { self!helper() }
    method !helper() { 41 }
    method boom() { EVAL 'return 1; self!nope() - self!helper()' }
    method fine() { EVAL 'self!helper() + 1' }
}

class Base { method b() { 7 } }   # resolves the stub, after the EVAL sites

my $w = Widget.new;

# (1) EVAL must not trip the outer stub check
is EVAL('$w.run'), 41, 'EVAL sees method; outer stub not yet defined does not error';
is $w.fine, 42, 'EVAL of a valid private call works';

# (2) typo'd private call errors at compile time despite the `return 1`
throws-like { $w.boom }, X::Method::NotFound,
    'typo private method in EVAL throws X::Method::NotFound';

# valid private dispatch still works
is $w.run, 41, 'valid private call dispatches';

# the forward-declared stub was defined and is usable
is Base.new.b, 7, 'forward-stubbed class resolved later in the unit';
