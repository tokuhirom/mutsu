use Test;
plan 5;

throws-like ｢temp $_ = 42; grep $_ == 1, 1,2,3｣, X::Match::Bool,
    'grep bool braino throws X::Match::Bool';
throws-like ｢temp $_ = 42; (1,2,3).grep: $_ == 1｣, X::Match::Bool,
    'method grep bool braino throws X::Match::Bool';

my @a = 1..10;
@a.grep(* %% 2).>>++;
is @a, <1 3 3 5 5 7 7 9 9 11>,
    'grep result stays rw-like for chained hyper postfix update';

is-deeply ("foo").grep({ /foo/ }), ("foo",),
    'block returning regex to grep is handled in list context';

is (^Inf).grep(*.is-prime).is-lazy, True,
    'grep over infinite range remains lazy';
