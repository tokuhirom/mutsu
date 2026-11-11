use lib 't/lib';
use Test;

# A regex code block parsed in a unit that activated a slang leaves the slang
# active for the rest of the unit (#16888, EEL 0.3's `t/44`: a `/ a { make 17 } /`
# before an `event` with `pattern` declarations).

plan 2;

use PatternDeclaratorSlang;

ok 'a' ~~ / a { 1 } /, 'a regex with a code block';

class Greeter {
    shout hello { hi }
}
is Greeter.hello, 'HI!', 'the slang declarator after it';
