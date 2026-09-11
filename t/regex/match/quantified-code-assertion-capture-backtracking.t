use Test;

plan 2;

# A code assertion after a quantified capture must be retried with the
# captures from the shorter candidate. Match values numify through their text,
# so `$0.sum` can select the candidate whose digits add up to 28.
my @sums;
my $match = "1 2 3 4 5 6 7 8 " ~~ /^ [(\d) \s]+ <?{ @sums.push($0.sum); $0.sum == 28 }>/;

is @sums, (36, 28), 'the assertion sees each backtracked capture list';
is ~$match, '1 2 3 4 5 6 7 ', 'the assertion accepts the shortened quantified match';
