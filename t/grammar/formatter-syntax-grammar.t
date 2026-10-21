use v6.e.PREVIEW;
use Test;

# `Formatter::Syntax` is a real Grammar (Rakudo's src/core.e/Formatter.rakumod),
# not a whole-string Match.
plan 20;

my $m = Formatter::Syntax.parse('%5d');
isa-ok $m, Match, 'parse answers a Match';
is $m<statement>.elems, 1, 'one statement';
my $d = $m<statement>[0]<directive>;
is $d<size>.Str, '5', 'size capture';
is $d<sym>.Str, 'd', 'sym capture';
is $d<flags>.elems, 0, 'no flags';

$m = Formatter::Syntax.parse('ab%-08.3fx%%y');
is $m<statement>.map({ .hash.keys.sort.join('+') }).join(' '),
    'literal directive literal directive literal', 'statement kinds';
my $f = $m<statement>[1]<directive>;
is $f<flags>.map(*.Str).join(','), '-,0', 'flags';
is $f<size>.Str, '8', 'size';
is $f<precision><size>.Str, '3', 'precision size';
is $f<sym>.Str, 'f', 'sym';
is $m<statement>[3]<directive>.Str, '%%', 'a percent directive';

$d = Formatter::Syntax.parse('%2$*3$d')<statement>[0]<directive>;
is $d<idx>.Str, '2$', 'argument index';
is $d<size><star>.Str, '*', 'star size';
is $d<size><idx>.Str, '3$', 'star size index';

is Formatter::Syntax.parse('%s')<statement>[0]<directive><sym>.Str, 's', 'a string directive captures its symbol';
ok Formatter::Syntax.parse('%v')<statement>[0]<panic>.defined, 'an unsupported letter is a panic';
nok Formatter::Syntax.parse('%').defined, 'a lone percent does not parse';
nok Formatter::Syntax.parse('100%').defined, 'a trailing percent does not parse';
is Formatter::Syntax.subparse('%5dxx').Str, '%5dxx', 'subparse';
is Formatter::Syntax.parse('').Str, '', 'the empty format parses';
