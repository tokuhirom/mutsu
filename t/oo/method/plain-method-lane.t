use Test;

# The CallMethodMut "plain method lane" (#8880) remembers, per (receiver class,
# method name), that the whole pre-dispatch probe chain declined the call, and
# skips the chain on every later call. These tests pin the cases where a probe
# MUST still win, or must start winning after a class changes shape -- i.e. the
# ones a stale or over-eager memo would break. Every case calls the same method
# more than once on purpose: the first call populates the memo, the second is
# the one that replays it.

plan 32;

# --- the lane itself: a plain user method stays correct across calls ---------
class Plain {
    has $.v;
    method double() { $!v * 2 }
}
my $p = Plain.new(v => 21);
is $p.double, 42, 'plain method, first call';
is $p.double, 42, 'plain method, second call (lane replay)';
is $p.double, 42, 'plain method, third call';

# --- the accessor probe must keep winning -----------------------------------
# `try_fast_accessor_read` claims `$p.v` before any user-method resolution, so
# the pair is never memoized; a memo that claimed it anyway would dispatch a
# method that does not exist.
is $p.v, 21, 'public accessor, first call';
is $p.v, 21, 'public accessor, second call';

# --- an explicit method of the same name as an attribute --------------------
class Shadow {
    has $.n;
    method n() { 'method wins' }
}
my $s = Shadow.new(n => 5);
is $s.n, 'method wins', 'explicit method shadows the accessor, first call';
is $s.n, 'method wins', 'explicit method shadows the accessor, second call';

# --- a proto body must keep intercepting ------------------------------------
class Proto {
    proto method pick-one($) {*}
    multi method pick-one(Int $x) { "int $x" }
    multi method pick-one(Str $x) { "str $x" }
}
my $pr = Proto.new;
is $pr.pick-one(1), 'int 1', 'proto dispatch, first call';
is $pr.pick-one('a'), 'str a', 'proto dispatch, other candidate';
is $pr.pick-one(2), 'int 2', 'proto dispatch, repeat';

# --- .^add_method after the memo is warm ------------------------------------
# Adding a method bumps the registry method generation, which drops the memo.
# Without that, `later` would keep resolving to whatever the class had before.
class Growing {
    method here() { 'here' }
}
my $g = Growing.new;
is $g.here, 'here', 'pre-existing method, warming the memo';
is $g.here, 'here', 'pre-existing method, replay';
Growing.^add_method('later', method () { 'later' });
is $g.later, 'later', 'method added after the memo was warm resolves';
is $g.here, 'here', 'the pre-existing method still resolves after the class grew';

# --- augment after the memo is warm -----------------------------------------
class Augmented {
    method base() { 'base' }
}
my $a = Augmented.new;
is $a.base, 'base', 'augmentable class, warming the memo';
use MONKEY-TYPING;
augment class Augmented {
    method extra() { 'extra' }
}
is $a.extra, 'extra', 'augmented method resolves after the memo was warm';

# --- a mixin gives the receiver a different class, so a different key -------
role Tagged {
    method double() { 'role double' }
}
my $mixed = Plain.new(v => 3);
$mixed does Tagged;
is $mixed.double, 'role double',
    'a mixin receiver does not replay the base class entry';

# --- wrapping a method after the memo is warm -------------------------------
# `push_method_wrap` bumps the method generation too, so the wrapper has to be
# seen by the very next call.
class Wrapped {
    method greet() { 'plain' }
}
my $w = Wrapped.new;
is $w.greet, 'plain', 'wrappable method, warming the memo';
is $w.greet, 'plain', 'wrappable method, replay';
Wrapped.^find_method('greet').wrap(-> | { 'wrapped ' ~ callsame() });
is $w.greet, 'wrapped plain', 'the wrapper is seen after the memo was warm';

# --- the same method name on two unrelated classes --------------------------
class Left  { method who() { 'left'  } }
class Right { method who() { 'right' } }
my $l = Left.new;
my $r = Right.new;
is $l.who, 'left',  'first class, first call';
is $r.who, 'right', 'second class, first call';
is $l.who, 'left',  'first class, replay';
is $r.who, 'right', 'second class, replay';

# --- a subclass inheriting the method ---------------------------------------
class Child is Plain {
    method triple() { $.v * 3 }
}
my $c = Child.new(v => 7);
is $c.double, 14, 'inherited method through a subclass, first call';
is $c.double, 14, 'inherited method through a subclass, replay';
is $c.triple, 21, 'the subclass own method, alongside the inherited one';

# --- an `is rw` accessor written through -------------------------------------
# The assignment form marks the call site as wanting a container ref, which the
# lane gate excludes; the read form does not.
class Mutable {
    has $.slot is rw;
}
my $m = Mutable.new(slot => 1);
is $m.slot, 1, 'rw accessor read, warming';
$m.slot = 9;
is $m.slot, 9, 'rw accessor write reaches the attribute';

# --- the same method with and without arguments ------------------------------
# The lane only ever applies to the zero-argument shape, so an optional
# parameter must still bind when one is passed.
class Optional {
    method label($tag = 'none') { "[$tag]" }
}
my $o = Optional.new;
is $o.label, '[none]', 'zero-arg call, warming';
is $o.label('x'), '[x]', 'one-arg call after the zero-arg memo';
is $o.label, '[none]', 'zero-arg call again';
