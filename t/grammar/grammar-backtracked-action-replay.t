use v6;
use Test;

plan 2;

class Actions {
    has Int $.count is rw = 0;
    method number($/) { $!count++ }
}

grammar G {
    rule TOP { <number> '!' || <number> }
    token number { \d+ }
}

my $actions = Actions.new;
my $match = G.subparse('42', :$actions);
ok $match && $match.pos == 2, 'the fallback branch completes the parse';
is $actions.count, 2,
    'actions from a rejected ordered-alternative branch are preserved';
