use v6;
use Test;

plan 30;

# IO::Path::Parts.new(\volume, \dirname, \basename) — direct construction.
my $p = IO::Path::Parts.new('C:', '/some/dir', 'foo.txt');
is $p.^name, 'IO::Path::Parts', '.^name is IO::Path::Parts';
is $p.volume, 'C:', '.volume accessor';
is $p.dirname, '/some/dir', '.dirname accessor';
is $p.basename, 'foo.txt', '.basename accessor';

# It does Associative, Positional and Iterable.
ok $p ~~ Associative, 'does Associative';
ok $p ~~ Positional, 'does Positional';
ok $p ~~ Iterable, 'does Iterable';

# Associative: <key> yields the part.
is $p<volume>, 'C:', '<volume> associative';
is $p<dirname>, '/some/dir', '<dirname> associative';
is $p<basename>, 'foo.txt', '<basename> associative';

# Positional: [i] yields the i-th part as an ordered Pair.
is $p[0].^name, 'Pair', '[0] is a Pair';
is $p[0].gist, 'volume => C:', '[0] is the volume pair';
is $p[1].gist, 'dirname => /some/dir', '[1] is the dirname pair';
is $p[2].gist, 'basename => foo.txt', '[2] is the basename pair';

# Rakudo's inherited single-item fallbacks deliberately itemize the object,
# despite IO::Path::Parts also exposing its three parts positionally.
is-deeply $p.list, ($p,), '.list contains self';
is-deeply $p.List, ($p,), '.List contains self';
is $p.elems, 1, '.elems is 1';
is-deeply $p.keys.List, (0,), '.keys contains the positional fallback key';
is-deeply $p.values.List, ($p,), '.values contains self';
is-deeply $p.pairs.map(*.gist).List,
    ('0 => IO::Path::Parts.new("C:","/some/dir","foo.txt")',),
    '.pairs contains 0 => self';
is $p.pairs[0].key.^name, 'Int', '.pairs fallback key is an Int';
is-deeply $p.kv.List, (0, $p), '.kv contains 0 and self';
my @seen;
for $p { @seen.push($_) }
is @seen.elems, 1, 'iteration visits once';
is @seen[0].raku, $p.raku, 'iteration visits self';

# .hash round-trips to a Map keyed by part name.
my %h = $p.hash;
is %h<basename>, 'foo.txt', '.hash keeps the parts';

# IO::Path.parts returns the same kind of object.
my $q = 'foo/bar.txt'.IO.parts;
is $q.^name, 'IO::Path::Parts', 'IO::Path.parts returns IO::Path::Parts';
is $q<dirname>, 'foo', '.parts dirname';
is $q<basename>, 'bar.txt', '.parts basename';
ok $q ~~ Associative, '.parts result does Associative';

# Named construction also works.
my $r = IO::Path::Parts.new(:volume(''), :dirname('a'), :basename('b'));
is $r.dirname, 'a', 'named-arg construction';

done-testing;
