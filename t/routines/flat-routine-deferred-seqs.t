use Test;

# The `flat` routine reads the still-deferred Seqs it descends into: a `map`
# whose blocks return `map` Seqs flattened to nothing (CSS::Units builds its
# dimension table this way, #10310).
plan 7;

is-deeply flat((1, 2).map: -> $x { ($x, $x + 10).map: { $_ * 2 } }).list,
    (2, 22, 4, 24), 'flat of a map of maps';
is-deeply flat((1, 2).map({ ($_, $_ + 10).map: { $_ => 1 } })).list,
    (1 => 1, 11 => 1, 2 => 1, 12 => 1), 'pairs';
is-deeply flat((1, 2).map({ ($_,) })).list, (1, 2), 'a map of Lists';
is-deeply flat(((1, 2), (3, 4))).list, (1, 2, 3, 4), 'plain nested Lists are unchanged';

my $s = (1, 2).map({ ($_, $_ + 10).map: { $_ * 2 } });
is-deeply flat($s).list, (2, 22, 4, 24), 'a $-held Seq operand';

enum Dim (pt => 1, pc => 12);
enum Ang (deg => 1);
my %table = flat((Dim, Ang).map: -> $dim { $dim.keys.map: { $_ => $dim } });
is-deeply %table<pt>, Dim, 'a table built from enum keys';
is-deeply %table<deg>, Ang, 'second enum';
