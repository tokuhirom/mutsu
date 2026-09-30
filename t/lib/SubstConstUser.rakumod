unit module SubstConstUser;
use SubstConstVars;
our constant $OWN = '-';
sub collapse(Str:D $s is copy) is export { $s ~~ s:g/ $WS ** 2..* /$WS/; $s }
sub imported-once(Str:D $s is copy) is export { $s ~~ s/ $WS /X/; $s }
sub own-const(Str:D $s is copy) is export { $s ~~ s:g/ $OWN /+/; $s }
