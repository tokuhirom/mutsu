use lib 't/lib';
use Test;

# A slang `proto` whose body is not `{*}` (#16888, EEL 0.3's
# "unsupported proto body" check): the body the slang's action built, holding
# an object it embedded with `RakuAST::Literal.from-value`, goes through the
# RakuAST frontend like any other and runs as the routine.

plan 2;

use PatternDeclaratorSlang;

class Solo {
    proto shout alone(|) { single }
}

is Solo.alone, 'SINGLE!', 'the proto body runs';
ok Solo.^find_method('alone').is_dispatcher, 'the proto is a dispatcher';
