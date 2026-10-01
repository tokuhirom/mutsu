use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

plan 3;

# `use worries` is a core pragma: it re-enables the compiler warnings an
# enclosing `no worries` turned off, for its own scope only (#10480).
{
    sub f { use worries; 1 }
    is f(), 1, '`use worries` inside a routine is accepted';
}

is_run ｢
    no worries;
    { use worries; my $ = :WorryFoo<> }
    my $ = :WorryBar<>;
    print "pass"
｣, %(:out<pass>, :err{
    .contains('use :WorryFoo') && !.contains('use :WorryBar')
}), '`use worries` re-enables warnings inside its block only';

is_run ｢use worries; my $ = :WorryFoo<>; print "pass"｣,
    %(:out<pass>, :err{ .contains: 'use :WorryFoo' }),
    'top-level `use worries` leaves warnings on';
