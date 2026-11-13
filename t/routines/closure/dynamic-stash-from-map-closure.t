use Test;

# `DYNAMIC::<$*x>` finds a caller's `my $*x` from a routine called with
# arguments and from a closure `map` runs, as `$*x` does (Red t/101 reads a
# `DYNAMIC::<$*RED-...>` flag in a TWEAK built inside a `map`).

plan 7;

sub with-arg($q) { DYNAMIC::<$*F> }
sub no-arg() { DYNAMIC::<$*F> }
sub in-map($q) { $q.map({ DYNAMIC::<$*F> }).List }
class It { has $.seen; submethod TWEAK { $!seen = DYNAMIC::<$*F> } }

{
    my $*F = 4;
    is with-arg(1), 4, 'from a routine called with an argument';
    is no-arg(), 4, 'from a routine called without one';
    is in-map((1, 2)), (4, 4), 'from a closure map runs';
    is (1,).map({ It.new.seen }).head, 4, 'from a TWEAK built inside a map';
}

nok no-arg().defined, 'absent outside the dynamic scope';
nok DYNAMIC::<$*NOPE>:exists, 'an undeclared name does not exist';
nok DYNAMIC::<$*OUT>.defined, 'a process-level dynamic lives in no frame';
