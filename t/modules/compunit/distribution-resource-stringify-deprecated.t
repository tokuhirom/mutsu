use v6;
use lib 't/lib/ResSplit/lib';
use Test;

plan 6;

# `~$r` and interpolation of a `%?RESOURCES` entry reach the deprecated `Str`
# from inside `Mu` in rakudo, so the notice's `seen at:` names that frame and
# not the user's line (#16095). A direct `.Str` keeps the user line (#16055).
use ResSplit;

my $r = strs();
my $viatilde = ~$r;
my $viainterp = "{$r}";
my $report = Deprecation.report // '';
ok $report ~~ / 'Method Str (from Distribution::Resource)' /, 'the notice names the method and class';
ok $report ~~ / 'SETTING::src/core.c/Mu.rakumod, line 867' /, 'it is seen at the Mu frame';
nok $report ~~ / 'line 12' /, 'the user line of ~$r is not named';
nok $report ~~ / 'line 13' /, 'nor the user line of the interpolation';
is $viatilde, $viainterp, 'both spellings stringify the same path';
ok $viatilde.chars > 0, 'and the path is not empty';
