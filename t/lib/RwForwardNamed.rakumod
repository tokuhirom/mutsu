unit module RwForwardNamed;

sub bump-named(str $t, $pos is rw, :$z) is export { $pos++; 5 }
sub bump-plain(str $t, $pos is rw) is export { $pos++; 5 }
