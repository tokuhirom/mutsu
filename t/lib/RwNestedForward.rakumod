unit module RwNestedForward;

sub bump(int $pos is rw, :$z) is export { $pos++; 5 }
sub nothing(int $pos is rw) is export { Nil }
sub outer-tail(int $pos is rw) is export { bump($pos) }
sub outer-chain(int $pos is rw) is export { nothing($pos) // bump($pos) }
sub outer-stmt(int $pos is rw) is export { bump($pos); 1 }
