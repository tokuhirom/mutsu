use Test;
plan 2;

grammar G {
    proto token word { * }
    multi token word { /ab/ }
    multi token word { /a/ }
}

$_ = "ab";
ok($_ ~~ G::word(), 'LTM picks longest match');

$_ = "a";
ok($_ ~~ G::word(), 'LTM picks shorter match');
