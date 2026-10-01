# A Regex value that closed over its own scope, spliced into another pattern
# (`<@r>` with a closure element), runs on the compiled regex engine
# (ADR-0135 Slice E): its body is compiled inline between a scope-enter and a
# scope-exit op whose effects backtracking undoes and redoes, so the body is
# matched lazily and its code sees the closure's lexicals. The compiled engine
# used to decline these patterns (`isolated-group-scoped`). Values verified
# against rakudo.
use Test;

plan 7;

sub mk { my $w = "yes"; rx/ b+ <?{ $w eq "yes" }> / }
my @r = mk();
is ~("abbc" ~~ / a <@r> c /), 'abbc', 'the closure lexical is visible to the body';

sub mk2 { my $w = "yes"; rx/ b+ <?{ $w eq "yes" }> / }
my @r2 = mk2();
is ~("abbbc" ~~ / a <@r2> bc /), 'abbbc', 'backtracking into the body re-installs the scope';

my @log;
sub mk3 { rx/ b+ <?{ @log.push($/.chars); True }> / }
my @r3 = mk3();
"abbbc" ~~ / a <@r3> bc /;
is @log.join(","), '3,2', 'the body is matched lazily: the assertion runs once per end tried';

my $w = "outer";
sub mk4 { my $w = "inner"; rx/ b <?{ $w eq "inner" }> / }
my @r4 = mk4();
ok "abc" ~~ / a <@r4> c /, 'the closure lexical shadows the caller\'s';
is $w, 'outer', 'and is uninstalled after the match';

sub mk5 { my $w = "no"; rx/ b <?{ $w eq "yes" }> / }
my @r5 = mk5();
nok "abc" ~~ / a <@r5> c /, 'a failing body unwinds its scope';

sub mk6 { my $n = 2; rx/ b ** {$n} / }
my @r6 = mk6();
is ~("abbc" ~~ / a <@r6> c /), 'abbc', 'a closure lexical used as a count';
