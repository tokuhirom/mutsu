use Test;

# From ML::TriesWithFrequencies (t/12-random-choice): each repetition of a Seq
# under `xx` is a List (Positional), also for a re-evaluated left-hand side.

plan 4;

my @w = (<a b c>.tail(*-1)) xx 3;
ok @w.all ~~ Positional, 'repetitions of a method-call Seq are Positional';
isa-ok @w[0], List, 'and are Lists';

my $n = 3;
my @v = (<a b c>.tail(*-1)) xx $n;
isa-ok @v[0], List, 'also with a runtime count';

sub f(@words) { so @words.all ~~ Positional }
ok f(@w), 'accepted by a @-parameter check';

done-testing;
