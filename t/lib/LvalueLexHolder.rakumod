unit module LvalueLexHolder;

class Slot { has $.n is rw = 0 }

my $state = Slot.new;

sub bump() is export { $state.n = 1; }
