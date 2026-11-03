use v6;
use lib 't/lib/ResSplit/lib';
use Test;

plan 3;

# `.Str` of a `%?RESOURCES` entry is deprecated in rakudo and records its
# notice at the call site (#16055).
use ResSplit;

my $r = strs();
my $s = $r.Str;
my $report = Deprecation.report // '';
ok $report ~~ / 'Method Str (from Distribution::Resource)' /, 'the notice names the method and class';
ok $report ~~ / 'Please use %?RESOURCES<key> directly instead.' /, 'the notice carries the replacement hint';
ok $report ~~ / 'line 12' /, 'the notice names the call site line';
