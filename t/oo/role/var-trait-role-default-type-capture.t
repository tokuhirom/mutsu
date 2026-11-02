use Test;
use nqp;

# Found via Hash::Sorted: `my %m is Role` puns a role whose type capture has a
# default (`::KeyT = str`). The capture must be bound to that default inside
# `new`, as `Role.new` binds it (it used to see the `%` variable's `Str`).
plan 6;

role Q[::T = Int] does Associative {
    has $.seen;
    method new() { self.bless(seen => T.^name) }
    method kt { T.^name }
    method STORE(\p, *%) { self }
}
my %n is Q;
is %n.seen, 'Int', '`new` sees the capture default under `my %h is Role`';
is %n.kt, 'Int', 'a method sees the capture default';
is Q.new.seen, 'Int', 'Role.new binds the same default';

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
is %m.keys.elems, 2, 'a typed key array built from the default capture takes the keys';
is %m.keys.WHAT.raku, 'array[str]', 'and it is the native str array';
is %m.keys.join(','), 'a,b', 'the keys are in the order splice at 0 leaves them';
