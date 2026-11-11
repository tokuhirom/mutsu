use Test;

# `.subparse` tries its start rule at the start (or `:pos`) only: a start rule that
# fails there is not retried at each later position, so its code blocks run once
# (#16888: the slang host's subparse of a production scanned the rest of the unit).

plan 8;

my @seen;
grammar G {
    token TOP { { @seen.push($/.from) } 'b' }
}

nok G.subparse('aab'), 'no match at 0 is a failed subparse';
is-deeply @seen, [0], 'the start rule was tried at 0 only';

@seen = ();
nok G.subparse('aab', :pos(1)), 'no match at :pos(1)';
is-deeply @seen, [1], 'tried at the :pos only';

@seen = ();
ok G.subparse('baa'), 'a match at 0 succeeds';
is G.subparse('baa').to, 1, 'and stops where the rule does';

grammar Dyn {
    token TOP { :my $*X = 'dyn'; 'p' <m> }
    method m { @seen.push($*X); self.n }
    token n { 'q' }
}
@seen = ();
ok Dyn.subparse('pq'), 'a method the start rule calls';
is-deeply @seen, ['dyn'], 'sees the rule\'s dynamic variable';
