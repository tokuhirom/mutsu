use Test;

# ADR-11276 §9.101: an allomorph (`IntStr`, `RatStr`, `NumStr`, `ComplexStr`)
# is answered by the method table on the part each row reads. Rakudo's MRO is
# `IntStr Allomorph Str Int Cool Any Mu`: `Str`'s and `Cool`'s text rows read
# the string, the numeric type's rows and `Cool`'s numeric ones the number,
# and `Any`'s list methods the allomorph itself as one element.

plan 34;

# Str's and Cool's text rows read the string part.
is <1e0>.flip, '0e1', 'Str.flip reads the string part';
is <0x10>.uc, '0X10', 'Str.uc reads the string part';
is IntStr.new(5, 'zero').chars, 4, 'Cool.chars reads the string part';
is-deeply <1e0>.comb.List, ('1', 'e', '0'), 'Str.comb reads the string part';
is <65>.uniprop('Script'), 'Common', 'Cool.uniprop($prop) reads the string part';
is <12>.indent(2), '  12', 'Cool.indent($n) reads the string part';
is <1.5>.substr(1), '.5', 'Str.substr($from) reads the string part';
ok <1e0>.contains('e'), 'Cool.contains reads the string part';
is <1e0>.Version.raku, 'v1.e.0', 'Str.Version reads the string part, as in Rakudo';
throws-like { <1>.Date }, X::Temporal::InvalidFormat, 'Str.Date parses the string part';
is <1e0>.naive-word-wrapper, '1e0', 'Str.naive-word-wrapper answers an allomorph';

# The numeric type's rows and Cool's numeric rows read the number.
is-approx IntStr.new(5, 'zero').sqrt, 5.sqrt, 'Cool.sqrt reads the numeric part';
is IntStr.new(5, 'zero').base(2), '101', 'Int.base reads the numeric part';
is <1+2i>.re, 1e0, 'Complex.re reads the numeric part';
is <1.5>.numerator, 3, 'Rat.numerator reads the numeric part';
ok <7>.is-prime, 'Int.is-prime reads the numeric part';
is IntStr.new(5, 'zero').succ, 6, 'succ reads the numeric part';
is <1e0>.Int, 1, 'Int reads the numeric part';
nok <0.0>.Bool, 'Bool reads the numeric part';

# Any's list methods read the allomorph itself as the one element.
is-deeply <1>.head, <1>, 'Any.head keeps the allomorph';
is-deeply IntStr.new(5, 'zero').max, IntStr.new(5, 'zero'), 'Any.max keeps the allomorph';
is-deeply <1.5>.sort.List, (<1.5>,), 'Any.sort keeps the allomorph';
is-deeply <1>.pick, <1>, 'Any.pick keeps the allomorph';
is-deeply <1>.serial, <1>, 'Any.serial is the allomorph';
is <1>.minmax.raku, 'IntStr.new(1, "1")..IntStr.new(1, "1")', 'Any.minmax keeps the allomorph';
is <1e0>.join, '1e0', 'Any.join stringifies the allomorph to its string part';

# The names an allomorph answers about itself.
is <1e0>.raku, 'NumStr.new(1e0, "1e0")', '.raku names the allomorph';
is <1e0>.gist, '1e0', '.gist is the string part';
is <1>.WHICH.Str, 'IntStr|Int|1|Str|1', '.WHICH names both parts';
nok <1>.ACCEPTS('1.0'), 'ACCEPTS compares a Str argument with the string part';
ok <1>.ACCEPTS(1.0), 'ACCEPTS compares a number with the numeric part';
is-deeply <1x>.wordcase, '1X', 'a word that is not a number wordcases to a Str';
is-deeply <0x10>.wordcase, IntStr.new(0, '0X10'), 'wordcase re-allomorphs, as in Rakudo';

# A role mixed into an allomorph keeps the mixin dispatch.
my $r = <42> but role { method uc { 'R' } };
is $r.uc, 'R', 'a role mixed into an allomorph answers its own method';
