use Test;

# `Grammar.parse` / `.subparse` with `:pos`: the match is anchored at that
# position however the argument is passed (a variable arrives in its item
# container), and a `.parse` from it must still reach the end (#16888, the
# slang host matches in the whole unit from the production's position).

plan 7;

grammar G { token foo { 'answer' } }

my $pos = 3;
is ~G.subparse('xx answer!', :rule<foo>, :pos(3)), 'answer', 'subparse :pos(N)';
is ~G.subparse('xx answer!', :rule<foo>, :$pos), 'answer', 'subparse :$pos';
is ~G.subparse('xx answer!', :rule<foo>, pos => $pos), 'answer', 'subparse pos => $p';
is G.subparse('xx answer!', :rule<foo>, :$pos).from, 3, 'the match starts there';
is ~G.parse('xx answer', :rule<foo>, :pos(3)), 'answer', 'parse :pos(N) to the end';
is ~G.parse('xx answer', :rule<foo>, :$pos), 'answer', 'parse :$pos';
nok G.parse('xx answer!', :rule<foo>, :$pos), 'parse :pos must still reach the end';
