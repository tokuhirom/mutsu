use Test;

# A leading `&` opens no empty conjunct: rakudo drops it, as it drops a
# leading `|` (#18713, roast S05-mass/rx.t).

plan 4;

ok 'a' ~~ /a |& b/, '|& — leading & inside an alternation branch';
ok 'b' ~~ /a |& b/, 'the second branch still matches';
ok 'a' ~~ /& a/, 'a leading & at the start of the regex';
ok 'b' ~~ /a ||& b/, 'after a sequential alternation';
