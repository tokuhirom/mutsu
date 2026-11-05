use Test;

# A code-free pattern that begins with a plain `$var` reads the variable once
# for the whole scan and only tries start positions where its text occurs.

plan 16;

my $needle = 'host';
my $l = 'a host3 b host14 c';
ok $l ~~ / $needle \d+ /, 'found past the first start positions';
is ~$/, 'host3', 'first occurrence';
is $/.from, 2, 'match start';

my @all = $l.match(/ $needle \d+ /, :g).map(~*);
is @all.join(','), 'host3,host14', ':g finds every occurrence';

my $ov = 'aa';
is 'aaaa'.match(/ $ov /, :ov).elems, 3, ':ov overlapping occurrences';
is 'aaaa'.match(/ $ov /, :g).elems, 2, ':g non-overlapping occurrences';

nok 'abc def' ~~ / $needle \d+ /, 'no occurrence';

my $empty = '';
ok 'abc' ~~ / $empty /, 'empty value matches zero-width';
is $/.from, 0, 'at the start';

my $num = 42;
ok 'x 42 y' ~~ / $num /, 'Int value is stringified';
is $/.from, 2, 'Int value start';

my $rx = / \d+ /;
ok 'ab 12' ~~ / $rx /, 'Regex value is a pattern';
is ~$/, '12', 'Regex value match';

my $special = 'a.b';
nok 'axb' ~~ / $special /, 'regex metacharacters in the value are literal';
ok 'xa.b' ~~ / $special /, 'literal value found';

my $w = 'été';
ok 'un été chaud' ~~ / $w /, 'non-ASCII value';

# vim: ft=raku
