use Test;

plan 10;

# A user-defined postfix with a precedence trait binds to the term it follows. The postfix
# loop used to skip *every* precedence-annotated postfix, on the theory that a prefix would
# pick it up later -- but nothing picks it up after a plain term, so `4.7k` was a parse error.
sub postfix:<k> ($a) is tighter(&infix:<*>) { $a * 1000 }

is 4.7k, 4700, 'a postfix with a precedence trait applies to a plain term';
is 2 * 3k, 6000, 'tighter(&infix:<*>) binds before the multiplication';

# A postfix declared looser than another postfix still chains left to right, even when both
# are alphabetic: `4kV` is postfix:<V>(postfix:<k>(4)).
sub postfix:<V> ($a) is looser(&postfix:<k>) { "V" ~ $a }
is 4kV, 'V4000', 'two alphabetic postfixes chain';

# `is looser(&postfix:<k>)` is relative to k's *registered* level, not to the default
# prefix level it would have had without its trait.
sub postfix:<W> ($a) is looser(&postfix:<V>) { "W" ~ $a }
is 4kVW, 'WV4000', 'a chain of three postfixes at descending levels';

# A postfix declared looser than a *prefix* still waits for the prefix to build its call.
sub prefix:<D> (Int $x) { 2 * $x }
sub postfix:<P> (Int $x) is looser(&prefix:<D>) { 1 + $x }
sub postfix:<Q> (Int $x) is tighter(&prefix:<D>) { 1 + $x }
is (D 3P), 7, 'a postfix looser than a prefix applies to the whole prefix call';
is (D 3Q), 8, 'a postfix tighter than a prefix applies to the prefix operand';

# `Mu.ACCEPTS`: `$x.ACCEPTS($y)` is the smartmatch `$y ~~ $x`. Plain scalars used to throw
# X::Method::NotFound.
ok 4000.ACCEPTS(4000.0), 'a numeric ACCEPTS smart-matches its argument';
nok 4000.ACCEPTS(5), 'a numeric ACCEPTS rejects a non-match';
ok 'a'.ACCEPTS('a'), 'a string ACCEPTS smart-matches its argument';

# `Range.new` builds the same range as `..`, adverbs included.
is-deeply (Range.new(1, 5), Range.new(1, 5, :excludes-max), Range.new(3700/1, 5700/1)).map(*.gist).List,
    ('1..5', '1..^5', '3700.0..5700.0'),
    'Range.new builds a range, with excludes-min/max';
