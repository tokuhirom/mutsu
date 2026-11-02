use Test;

# #15991: `my %h is Role` / `my @a is Role` puns a parametric role with its
# type-parameter defaults bound, as `Role.new` does. The expected values are
# rakudo's.

plan 9;

role Q[::T = Int] does Associative {
    has $.seen;
    method new() { self.bless(seen => T.^name) }
    method STORE(\p, *%) { self }
}
my %n is Q;
is %n.seen, 'Int', '%h is Q binds the default';
is Q.new.seen, 'Int', 'Q.new binds the same default';

role Two[::T = Rat, ::U = Int] does Associative {
    has $.seen;
    method new() { self.bless(seen => T.^name ~ ' ' ~ U.^name) }
    method STORE(\p, *%) { self }
}
my %two is Two;
is %two.seen, 'Rat Int', 'two defaulted type parameters';

role P[::T = Str] does Positional {
    has $.seen;
    method new() { self.bless(seen => T.^name) }
    method STORE(\p, *%) { self }
}
my @p is P;
is @p.seen, 'Str', '@a is P binds the default';

my %explicit is Q[Num];
is %explicit.seen, 'Num', 'an explicit argument still wins';

use nqp;
my sub typed-array(Mu \type) { nqp::objprimspec(type) ?? array[type] !! Array[type] }
role R[::KeyT = str] does Associative {
    has @.keys;
    method new() {
        my $self := nqp::create(self);
        nqp::bindattr($self, self, '@!keys', typed-array(KeyT).new);
        $self
    }
    method add($k) { @!keys.splice(0, 0, $k); @!keys.elems }
    method STORE(\pairs, *%) { for pairs { self.add(.key) }; self }
}
my %m is R = b => 42, a => 666;
is %m.keys.WHAT.raku, 'array[str]', 'a native default makes a native array';
is %m.keys.join(','), 'a,b', 'and the keys are stored';

role Plain[::T = Int] {
    has $.seen;
    method new() { self.bless(seen => T.^name) }
}
is Plain.new.seen, 'Int', 'a role used without a tie is unchanged';

role NoParam does Associative {
    method STORE(\p, *%) { self }
}
my %plain is NoParam;
ok %plain ~~ NoParam, 'a role without type parameters still ties';
