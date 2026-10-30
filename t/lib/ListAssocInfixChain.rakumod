unit module ListAssocInfixChain;

sub infix:<lsx>( *@args ) is assoc<list> is export { "LS(" ~ @args.join(",") ~ ")" }
sub infix:<l3x>( $a, $b, $c? ) is assoc<list> is export { "L3($a,$b," ~ ($c // "-") ~ ")" }
multi sub infix:<lmx>( *@args ) is assoc<list> is export { "LM(" ~ @args.join(",") ~ ")" }
