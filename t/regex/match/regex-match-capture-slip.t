use Test;
use lib 't/fixtures/regex-hyphen-lexical/lib';
use Regex::Hyphen::Fixture;

# Lingua::Stem::Russian uses `|($word ~~ $RVRE)` to unpack a Match's
# positional captures while stemming a word.
plan 5;

my $match = 'a12' ~~ /(a)(\d+)/;
my ($first, $second) = |$match;
is $first, 'a', 'a slipped Match contributes its first positional capture';
is $second, '12', 'a slipped Match contributes its second positional capture';

my $named = 'ab' ~~ /$<letter>=(.)/;
my $seen;
sub takes-capture(|c) {
    $seen = c.hash<letter>;
}
takes-capture(|$named);
is $seen.Str, 'a', 'a slipped Match contributes named captures';
is $named.list.elems, 0, 'named-only Match has no positional captures';

ok derivational('радость'),
    'regex closures capture lexical variables whose names contain hyphens';
