use v6;
use Test;

# `infix:<...>` names its operator with an angle quote, whose ONLY escapes are
# the backslash itself and the delimiters. So `infix:<\\>` declares the
# ONE-character operator `\`, and `infix:<\n>` keeps both of its characters.
# mutsu kept the escape in the registered name, so MIDI::Make's
# `sub infix:<\\> (UInt8 $n, Pow2 $d)` declared a two-character operator that
# its own `4\4` default argument could never match. Refs #7954.
plan 7;

sub infix:<\\> ($a, $b) { "$a/$b" }

is &infix:<\\>.name, 'infix:<\>', 'the escape is resolved in the registered name';
is 4\4, '4/4', 'the one-character operator is reachable from source';
is (3\4), '3/4', 'and inside parentheses';

# MIDI::Make reaches it from a parameter default, where the whole signature
# failed to parse while the operator went by a name nothing spelled.
sub time-signature ($ts = 4\4) { $ts }
is time-signature(), '4/4', 'usable as a parameter default';
is time-signature(6\8), '6/8', '... and still overridable';

# A backslash before anything else is not an escape: both characters stay.
sub infix:<\n> ($a, $b) { "$a+$b" }
is &infix:<\n>.name, 'infix:<\n>', 'a non-delimiter backslash pair is left alone';
is (1 \n 2), '1+2', '... and the two-character operator still parses';

done-testing;
