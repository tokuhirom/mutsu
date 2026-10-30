unit role WhereSubRole;

sub big($x) { $x > 5 }
sub term:<Whole> () { Int }

multi method size ( $v where { big($_) } ) { "big" }
multi method size ( $v ) { "small" }

method kind ( $v where { $_ ~~ Whole } ) { "int" }
method plain ( $v ) { big($v) ?? "big" !! "small" }
