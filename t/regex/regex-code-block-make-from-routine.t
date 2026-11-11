use Test;
use nqp;

# A routine called from a regex code block can `make` on the block's `$/`
# (#16888, EEL 0.3's `&make` finds it through `nqp::getlexcaller`).

plan 5;

sub via-method($x) { nqp::decont(nqp::getlexcaller('$/')).make($x) }
sub via-bindattr($x) {
    nqp::bindattr(nqp::decont(nqp::getlexcaller('$/')), Match, '$!made', $x)
}

is ('a' ~~ / a { make 17 } /).made, 17, 'core make in a code block';
is ('a' ~~ / a { via-method 20 } /).made, 20, '.make from a called routine';
is ('a' ~~ / a { via-bindattr 21 } /).made, 21, 'bindattr $!made from a called routine';
is ('ab' ~~ / a { make 1 } b { via-method $/.made + 1 } /).made, 2,
    'a later block reads and replaces what an earlier one made';
is ('ab' ~~ / a { via-method 5 } b { } /).made, 5,
    'a block that makes nothing keeps the earlier make';
