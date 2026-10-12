use Test;

# `my Array enum` members are also Arrays (PDF::Content::Color's ColorName).

plan 9;

my Array enum CN is export(:x) «
    :Aqua[0, 1, 1] :Blue[0, 0, 1]
   »;

ok Blue ~~ List, 'a member matches List';
ok Blue ~~ Array, 'a member matches Array';
is Blue.^name, 'CN', 'it is still a CN';
sub f(List $l) { 'list' }
is f(Blue), 'list', 'it binds a List parameter';
is Blue.elems, 3, '.elems of the base';
is Blue[2], 1, 'positional subscript reads the base';
is Blue.max, 1, '.max of the base';
sub g(\a, \b, \c) { "$(a) $(b) $(c)" }
is g(|Blue), '0 0 1', 'slipping a member spreads the base';
is Aqua.join('-'), '0-1-1', '.join of the base';
