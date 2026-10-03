use v6;
use Test;

# GH #11371: an identifier followed by `=>` is always an autoquoted pair key,
# so a quote-language name (`s`, `m`, `tr`, `q`, ...) written with no space
# before the fat arrow never opens a quote with `=` as its delimiter -- even
# when a later `=` on the same statement could close it.

plan 7;

my %a = tr=>4, q=>5;
is-deeply %a, { tr => 4, q => 5 }, 'tr=> and q=> before another =>';

my %b = s=>1, m=>2, y=>3, tr=>4, q=>5, qq=>6, rx=>7, qw=>8, Q=>9, qx=>10, S=>11, TR=>12;
is %b.keys.sort.join(','), 'Q,S,TR,m,q,qq,qw,qx,rx,s,tr,y', 'every quote-language name as a key';
is %b<s> + %b<TR>, 13, 'the values are the pair values';

class C { has @.s }
my $first = C.new(s=>["a"]);
my $second = C.new(s=>["b"]);
is-deeply $second.s, ["b"], 'a named argument s=> with a later = on the line';

my %c = s	=>1, m =>2;
is-deeply %c, { s => 1, m => 2 }, 'horizontal whitespace before =>';

my $x = 'a=b=';
$x ~~ s=a=c=;
is $x, 'c=b=', 's=...=...= with = as the delimiter still substitutes';

is q=x=, 'x', 'q=...= with = as the delimiter still quotes';
