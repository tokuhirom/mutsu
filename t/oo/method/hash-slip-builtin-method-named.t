use Test;
plan 6;

my %a = p => True;
my @l = 3, 9, 2;
is-deeply @l.max(|%a), (1 => 9,), '|%hash reaches max as :p';
is-deeply @l.max(|%(p => True)), (1 => 9,), '|%(...) reaches max as :p';
is-deeply @l.min(|%a), (2 => 2,), '|%hash reaches min as :p';
is-deeply @l.max(|%a, :k), (1,), 'explicit :k overrides';
my $h = {p => True};
is-deeply @l.max(|$h), (1 => 9,), '|$hash';
is-deeply @l.max(|(p => True)), (1 => 9,), 'Pair splat still works';
