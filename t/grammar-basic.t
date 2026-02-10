use Test;
plan 2;

grammar G {
    token word { /ab/ }
    rule digit { /\d/ }
}

ok "ab" ~~ G::word(), 'token returns regex';
ok "7" ~~ G::digit(), 'rule returns regex';
