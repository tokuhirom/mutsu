use Test;
plan 8;

my $str = "abrAcadAbbra";
my @expected-str = <abrAcadAbbra AcadAbbra adAbbra Abbra>;
my @expected-pos = (0, 3, 5, 7);

ok $str ~~ m:i:overlap/ a (.+) a /, 'm:overlap with capture matches';
is +@$/, 4, '@$/ contains all overlap matches';
is $/.list.join('|'), @expected-str.join('|'), '$/.list has expected strings';
is $/.list.map(*.from), @expected-pos, '$/.list tracks start offsets';
is $/.list»[0].join('|'), @expected-str.map(*.substr(1,*-1)).join('|'), 'hyper index over captures';

my @ov = $str.match(/a .* a/, :ov).list;
is +@ov, 2, '.match :ov returns one best match per start';
is ~@ov[0], 'abrAcadAbbra', 'first :ov match';
is ~@ov[1], 'adAbbra', 'second :ov match';
