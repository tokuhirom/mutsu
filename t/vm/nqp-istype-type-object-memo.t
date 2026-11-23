use Test;
use nqp;

# `nqp::istype` is handed a type OBJECT. A repeat check of a built-in scalar
# answers from the type-check memo without asking the scope what the type's
# name means. The answers must stay the ones the full check gives, including
# when a lexical class or a constant reuses a core type's name.

plan 9;

my sub is-failure($x) { nqp::istype($x, Failure) }

is is-failure(1), 0, 'an Int is not a Failure';
is is-failure(1), 0, 'the same check again, answered from the memo';
is is-failure(Failure.new), 1, 'a Failure is a Failure';
is is-failure("a"), 0, 'a Str is not a Failure';

{
    my class Int { }
    my sub lexical($x) { nqp::istype($x, Int) }
    is lexical(5), 0, 'a core Int is not the lexical class named Int';
    is lexical(Int.new), 1, 'an instance of the lexical Int is one';
}

my sub global-int($x) { nqp::istype($x, Int) }
is (global-int(5), global-int(5), global-int('x')).join(','), '1,1,0',
    'outside that block, Int is the core Int again';

{
    my constant Str = 42;
    nok 'abc' ~~ Str, 'a constant named Str shadows the type for ~~';
}
is (nqp::istype('abc', Str), 'abc' ~~ Str).join(','), '1,True',
    'and outside its block both checks see the type';
