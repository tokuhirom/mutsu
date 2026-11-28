use Test;

# The user-`new` lane (`vm_user_new_lane`): once `Class.new(named...)` has
# walked the whole `CallMethodMut` chain into the class's own
# `method new(*%_)`, later calls go straight into its frame. Every replay
# must run the method as the chain does, and anything the memo does not
# prove must take the chain.

plan 12;

class D {
    has $.name;
    has %.provides;
    has @.depends;
    has %.meta;
    method new(*%_) { self.bless(|%_, :meta(%_)) }
}
for ^3 {
    my $d = D.new(name => "n$_", provides => { a => 1 }, depends => [1, 2]);
    is-deeply ($d.name, $d.provides, $d.depends, $d.meta<name>),
        ("n$_", { a => 1 }, [1, 2], "n$_"), "replay $_ runs the user new";
}

# The body's own effects: a counter in the class, and an outer lexical.
my $made = 0;
class C {
    has $.v;
    method new(*%h) { $made++; self.bless(|%h) }
}
C.new(v => $_) for ^4;
is $made, 4, 'an explicit *%h new runs its body on every replay';
is C.new(v => 9).v, 9, 'and binds its slurpy';

# A Mu value takes the full binder (the gate refuses it) and still binds.
class M { has $.m; method new(*%_) { self.bless(|%_) } }
M.new(m => 1) for ^3;
is M.new(m => Mu).m.raku, 'Mu', 'a Mu named argument binds through the slurpy';

# A positional argument is outside the lane: the new refuses it.
class P { has $.p; method new(*%_) { self.bless(|%_) } }
P.new(p => 1) for ^3;
dies-ok { P.new(1) }, 'a positional argument still dies';

# A spread at the call site takes the chain and still works.
my %args = p => 5;
is P.new(|%args).p, 5, 'a spread call site still constructs';

# A multi new is outside the lane.
class Multi {
    has $.x;
    multi method new(Int :$x!) { self.bless(:x($x * 2)) }
    multi method new(*%_) { self.bless(|%_) }
}
Multi.new(x => 1) for ^3;
is Multi.new(x => 4).x, 8, 'a multi new still picks its candidate';
is Multi.new(x => 'a').x, 'a', 'and the other one';

# Subclass inheriting the parent's new: keyed by the class called on.
class Kid is D { has $.k }
my $kid;
$kid = Kid.new(name => 'k', k => 3) for ^3;
is ($kid.WHAT.^name, $kid.name, $kid.k).join(','), 'Kid,k,3', 'a subclass runs the inherited new';

# A wrapped new runs its wrapper.
class W { has $.w; method new(*%_) { self.bless(|%_) } }
W.new(w => 1) for ^3;
my $wrapped = 0;
my $h = W.^find_method('new').wrap(-> |c { $wrapped++; callsame });
W.new(w => 2);
$h.restore;
is $wrapped, 1, 'a wrapped new runs the wrapper';
