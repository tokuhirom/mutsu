use Test;
plan 3;

ok "abc" ~~ /<a>/, 'named capture matches';
is $<a>, "a", 'capture variable set';

my $s = "abc";
$s.match(/<b>/);
is $<b>, "b", 'capture set via .match';
