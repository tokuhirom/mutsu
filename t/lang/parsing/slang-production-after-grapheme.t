use lib 't/lib';
use Test;

# A slang production after a grapheme of several code points (a ZWJ emoji is
# one grapheme of three) ends where it ends. The slang host reports the end of
# a production as a Match's `.to`, which counts graphemes, while the parser
# counts code points, so every such grapheme earlier in the unit used to shift
# where the parser resumed (#17486; EEL 0.3's t/38 and t/39). Checked against
# rakudo (RakuAST frontend).

plan 4;

use PatternDeclaratorSlang;

my $one = '👩‍💻';
class Greeter {
    shout hello(Int $x, $y) { hi }
    method plain { 42 }
}
is Greeter.hello(1, 2), 'HI!', 'a production after one ZWJ grapheme';
is Greeter.plain, 42, 'and the code after it';

my @more = '👩‍💻', '👩‍👩‍👧', "e\x[301]";
class Waver {
    shout bye { there }
}
is Waver.bye, 'THERE!', 'a production after several of them';
is (1, 2).map({ $_ * 2 }).List, (2, 4), 'a block after them parses as usual';
