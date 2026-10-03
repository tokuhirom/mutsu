use v6;
use Test;

plan 8;

grammar G {
    rule TOP { <word>+ % <op=.c>? x }
    rule word { <.alpha>+ }
    token c { '+' }
}

ok G.subparse('A x'), 'the last item gives back the literal after sigspace';
ok G.subparse('A B x'), 'the separated quantifier gives back its last item';
ok G.subparse('A+B x'), 'a present optional separator also backtracks';
ok G.subparse('A+B+C x'), 'several separated items backtrack';

grammar Ratcheted {
    rule TOP { <word>+: % <op=.c>? x }
    rule word { <.alpha>+ }
    token c { '+' }
}
nok Ratcheted.subparse('A x'), 'an explicit ratchet still commits the quantifier';

grammar Comma {
    rule TOP { <word>+ % ',' x }
    rule word { <.alpha>+ }
}
ok Comma.subparse('A,B x'), 'a mandatory separator can give back the last item';

grammar Bounded {
    rule TOP { <word> ** 1..3 %% ',' x }
    rule word { <.alpha>+ }
}
ok Bounded.subparse('A,B x'), 'a bounded separated quantifier backtracks';
ok Bounded.subparse('A,B, x'), 'a trailing %% separator still matches';
