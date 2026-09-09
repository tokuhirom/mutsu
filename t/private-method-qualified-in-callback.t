use Test;

# A qualified private call (`$o!Owner::meth`) is permitted LEXICALLY: what
# matters is the class the call is written inside, not the class whose method
# happens to be running when control reaches it. mutsu decided it from
# `method_class_stack` — the *dynamic* caller — so a call written inside a
# closure that another object calls back into was refused with
# "does not trust <that other class>". The unqualified spelling already had the
# captured-`self` fallback; only the qualified one was missing it.
# Every assertion below also passes under rakudo.

plan 8;

class Holder {
    has &.cb is rw;
    method set-cb(&c) { &!cb = &c }
    method run(|c) { &!cb(|c) }
}

class Owner {
    method !secret($n = 1) { "SECRET$n" }

    method via-closure() {
        my $self = self;
        my $h = Holder.new;
        $h.set-cb(sub () { $self!Owner::secret() });
        $h.run;
    }

    method via-closure-with-args() {
        my $self = self;
        my $h = Holder.new;
        $h.set-cb(sub ($n) { $self!Owner::secret($n) });
        $h.run(7);
    }

    method direct() { self!Owner::secret() }

    method unqualified-via-closure() {
        my $h = Holder.new;
        $h.set-cb(sub () { self!secret() });
        $h.run;
    }
}

is Owner.new.direct, 'SECRET1', 'a qualified private call in the owner itself still works';
is Owner.new.via-closure, 'SECRET1',
        'a qualified private call inside a closure another class invokes';
is Owner.new.via-closure-with-args, 'SECRET7', 'same, with arguments threaded through';
is Owner.new.unqualified-via-closure, 'SECRET1',
        'the unqualified spelling keeps working (it already had the fallback)';

# The permission check must still bite where it should: a class that really is
# not the owner and is not trusted cannot reach in. rakudo refuses this at
# COMPILE time, so it has to be built in a string rather than written inline.
eval-dies-ok q:to/CODE/, 'an untrusted class calling in is still refused';
    class Owner2 { method !secret() { 'S' } }
    class Intruder { has $.o; method poke() { $!o!Owner2::secret() } }
    Intruder.new(o => Owner2.new).poke;
    CODE

class Trusted { ... }
class Trusting {
    trusts Trusted;
    method !inner() { 'INNER' }
}
class Trusted {
    method reach($t) { $t!Trusting::inner() }
}
is Trusted.new.reach(Trusting.new), 'INNER', 'an explicit `trusts` still grants access';

# A SUBCLASS is not the owner. Naming the owner out loud is exactly how a
# subclass tries to reach a parent's private method, and Raku refuses it unless
# the parent `trusts` the subclass — so the lexical fallback above must key on
# the captured `self` being an instance of the owner ITSELF, not merely of
# something that inherits from it. (`roast/integration/advent2011-day11.t` pins
# the same rule; a looser fallback silently granted the subclass access.)
class Parent { method !hidden() { 'HIDDEN' } }
class Child is Parent {
    method reach()   { EVAL 'self!Parent::hidden()' }
    method public()  { 'PUBLIC' }
}
is Child.new.public, 'PUBLIC', 'an ordinary inherited call is unaffected';
throws-like { Child.new.reach }, X::Method::Private::Permission,
        'a subclass still cannot reach its parent\'s private method';
