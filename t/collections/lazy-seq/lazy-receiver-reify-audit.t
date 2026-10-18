use Test;

# The reify-table audit of ADR-13891 §3: on an infinite sequence Rakudo
# answers `.keys` and `.item` without reifying it (every answer checked
# against Rakudo).

plan 12;

sub endless { (1..*).map(* + 0) }

is endless.keys.head(3), (0, 1, 2), '.keys of an infinite sequence is its indices';
ok endless.keys.is-lazy, '... and stays lazy';
is (gather { take $_ for 5..7 }).keys, (0, 1, 2), '.keys of a finite gather';

is endless.item.head(2), (1, 2), '.item of an infinite sequence reads on';
is endless.item.VAR.^name, 'Scalar', '... in a Scalar container';
ok endless.item.is-lazy, '... and stays lazy';
is (gather { take 1; take 2 }).item.elems, 2, '.item of a finite gather';

my @a = 1, (1..3).map(* + 0).item;
is @a.elems, 2, 'an itemized lazy sequence is one element';

# An itemized lazy sequence held in a variable reads and renders as the bare one.
my $i = endless.item;
is $i.head(3), (1, 2, 3), '.head of a held itemized sequence';
my $k = endless.item;
like $k.raku, /^ '$((1, 2, 3, ' .* '100...).lazy.Seq)' $/, '.raku of an itemized infinite sequence';
is (1..3).map(* + 0).lazy.item.raku, '$((1, 2, 3).lazy.Seq)', '.raku of an itemized .lazy sequence';
is (gather { take 1; take 2 }).item.raku, '$((1, 2).Seq)', '.raku of an itemized gather';
