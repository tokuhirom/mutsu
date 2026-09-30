use Test;
use nqp;

# An `is repr('VMArray')` IterationBuffer subclass that makes itself
# immutable the way the ValueList / Tuple distributions do: the positional
# protocol methods are installed with `^add_method` and throw. Its slots hold
# the stored objects themselves, not Scalar containers, so neither a bind
# into it nor a loop over it may write. Pinned against rakudo (#10261).

plan 14;

my class VL is IterationBuffer does Positional does Iterable is repr('VMArray') {
    method new(VL: +@args) { nqp::create(self)!SET-SELF(@args.iterator) }
    method STORE(VL:D: \to-store, :$INITIALIZE) {
        $INITIALIZE
          ?? self!SET-SELF(to-store.iterator)
          !! X::Assignment::RO.new(value => self).throw
    }
    method !SET-SELF(\iterator) is raw {
        nqp::until(
          nqp::eqaddr((my \pulled := iterator.pull-one), IterationEnd),
          nqp::push(self, nqp::decont(pulled))
        );
        my $vl = self
    }
    method list() { self.List }
    BEGIN for <ASSIGN-POS BIND-POS push> -> $method {
        VL.^add_method: $method, method (VL:D: |) {
            X::Immutable.new(:$method, typename => self.^name).throw
        }
    }
}

my @a is VL = ^3;

throws-like { @a[0] = 42 }, X::Immutable, 'assigning an element calls the ASSIGN-POS override';
throws-like { @a[0] := 42 }, X::Immutable, 'binding an element calls the BIND-POS override';
throws-like { @a.BIND-POS(0, 42) }, X::Immutable, 'a direct BIND-POS call also reaches it';
dies-ok { $_ = 42 for @a }, 'the iterated topic is not assignable';
dies-ok { for @a.kv -> \k, \v { v = 42 } }, 'a sigilless .kv value is not assignable';
dies-ok { for @a -> \v { v = 42 } }, 'a sigilless loop parameter is not assignable';
is-deeply @a.List, (0, 1, 2), 'nothing was written';

is-deeply @a.kv.List, (0, 0, 1, 1, 2, 2), '.kv answers from the class\'s own .list';
is-deeply @a.keys.List, (0, 1, 2), '.keys too';
is-deeply @a.pairs.List, (0 => 0, 1 => 1, 2 => 2), '.pairs too';

my $b = VL.new(^3);
dies-ok { $_ = 42 for $b.list }, 'a loop over .list of a scalar-held buffer';
throws-like { $b[0] := 42 }, X::Immutable, 'binding through a scalar-held buffer';

# A mutable Array keeps its element containers.
my @m = 1, 2;
$_ = 9 for @m;
is-deeply @m, [9, 9], 'a real Array is still written through';

# A class with a `list` override but no Positional storage.
class L { method list { (7, 8) } }
is-deeply L.new.kv.List, (0, 7, 1, 8), 'Any.kv is self.list.kv';
