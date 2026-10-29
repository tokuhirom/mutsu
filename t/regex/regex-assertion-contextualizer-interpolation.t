use Test;

plan 14;

# `<$( code )>` / `<@( code )>` interpolate the value of an expression as an
# indirect subrule (strings are regex source), and `<name=...>` aliases it.
# MUGS::Games' adventure CommandParser writes `<thing=@($*ctx.items)>`.
my $*items = <apple pear>;
my @re = <a.c>;
my $re = 'a.c';

ok 'pear' ~~ /<@($*items)>/, '<@(code)> matches an element';
ok 'abc' ~~ /<@(@re)>/, '<@(code)> reads a string element as regex source';
nok 'abc' ~~ /@(@re)/, 'the bare @(code) form stays literal';
ok 'abc' ~~ /<$($re)>/, '<$(code)> reads a string as regex source';
nok 'abc' ~~ /$($re)/, 'the bare $(code) form stays literal';

ok 'pear' ~~ /<thing=@($*items)>/, 'an aliased <name=@(code)> matches';
is ~$<thing>, 'pear', '... and names the match';
ok 'xyz' ~~ /<w=$('x' ~ 'y')>/, 'an aliased <name=$(code)> matches';
is ~$<w>, 'xy', '... and names the match';

my @plain = <apple pear>;
ok 'pear' ~~ /<t=@plain>/, 'an aliased <name=@array> matches';
is ~$<t>, 'pear', '... and names the match';

# A lazy result (`.grep` / `.map`) is read whole, for the bare form too.
ok 'n' ~~ /<@(<n e>.grep(?*))>/, '<@(code)> over a lazy .grep result matches';
ok 'n' ~~ /@(<n e>.map({ $_ }))/, 'the bare @(code) over a lazy .map result matches';
my %abbr = north => 'n', east => 'e';
ok 'n' ~~ /<s=@(%abbr{<north east>}.grep(?*))>/, 'an aliased <name=@(code)> over a hash slice .grep matches';

done-testing;
