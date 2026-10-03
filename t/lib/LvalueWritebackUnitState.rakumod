unit module LvalueWritebackUnitState;
class State { has $.top is rw = 0; }
my $state = State.new;
our sub set-top($n) is export { $state.top = $n; }
our sub get-top() is export { $state.top }
