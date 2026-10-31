use Test;

# `has $a OP= EXPR` declares the alias attribute `$!a`; the compound
# assignment half is compiled where the declaration is written, which may be a
# method body, and must reach the attribute (#15563). The expected names are
# rakudo's.

plan 5;

class P { method m { has $a ||= 5; 1 } }
is P.^attributes.map(*.name).join(","), '$!a', 'an alias attribute with ||= in a method body';

class P2 { method m { has $b //= 5; 1 } }
is P2.^attributes.map(*.name).join(","), '$!b', 'an alias attribute with //= in a method body';

class P3 { method m { has $c += 5; 1 } }
is P3.^attributes.map(*.name).join(","), '$!c', 'an alias attribute with += in a method body';

class P4 { method m { has $!d ||= 5; 1 } }
is P4.^attributes.map(*.name).join(","), '$!d', 'a private attribute with ||= in a method body';

class P5 { method m { has $.e += 5; 1 } }
is P5.^attributes.map(*.name).join(","), '$!e', 'a public attribute with += in a method body';

done-testing;
