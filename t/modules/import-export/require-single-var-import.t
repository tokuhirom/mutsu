use Test;

# #14340: a block-level `require M <$var>` whose list names only a sigiled
# variable declares that variable in the block (`<$x>` is one string literal,
# not a list).
use lib 'roast/packages/S11-modules/lib';

plan 2;

{
    require GH2983 <$question>;
    is $question.chars, 59, 'single listed variable is declared in the block';
}

{
    require GH2983 <$question C2983>;
    is C2983, 42, 'a variable listed with a term still works';
}
