use Test;

# #9212: a `?` that takes its zero branch over a subrule CALL -- a capturing
# lookaround (`<after x>`, `<before y>`) or a builtin subrule (`<alpha>`), bare
# or under a `$<a>=` alias -- never ran the call, so it publishes no capture.
# A `$<a>=` alias on a plain atom (`[y]`, `<[cd]>`) still renders an empty
# Match on the zero branch, as raku does.

plan 22;

{
    my $m = "xx" ~~ /<after x>? x/;
    is ~$m, 'x', '<after x>? x matches at 0';
    nok $m<after>:exists, 'the skipped <after x> publishes no capture';
}
{
    my $m = "xx" ~~ /$<a>=<after x>? x/;
    nok $m<a>:exists, 'an alias on a skipped <after x> publishes no alias';
    nok $m<after>:exists, 'nor the lookaround name';
}
{
    my $m = "xx" ~~ /$<a>=<?after x>? x/;
    nok $m<a>:exists, '$<a>=<?after x>? publishes no alias when skipped';
    nok $m<after>:exists, 'nor an after capture';
}
{
    my $m = "yx" ~~ /$<a>=<?after y>? x/;
    is ~$m, 'x', 'control: the assertion matches after y';
    ok $m<a>:exists, 'so the alias is published';
    ok $m<after>:exists, 'and the lookaround name is published';
}
{
    my $m = "xx" ~~ /<before y>? x/;
    nok $m<before>:exists, 'a skipped <before y> publishes no capture';
    $m = "ab" ~~ /<before a>? a/;
    ok $m<before>:exists, 'a matched <before a> publishes its capture';
}
{
    my $m = "1" ~~ /<alpha>? 1/;
    is ~$m, '1', '<alpha>? 1 matches';
    nok $m<alpha>:exists, 'a skipped builtin subrule publishes no capture';
    $m = "1" ~~ /$<a>=<alpha>? 1/;
    nok $m<a>:exists, 'nor under a $<a>= alias';
    nok $m<alpha>:exists, 'nor its own name under the alias';
    $m = "c1" ~~ /<alpha>? 1/;
    is ~$m<alpha>, 'c', 'a builtin subrule that does run is captured';
    $m = "1" ~~ /<alpha>?? 1/;
    nok $m<alpha>:exists, 'the frugal ?? zero branch publishes nothing either';
}
{
    my $m = "1" ~~ /$<a>=[y]? 1/;
    ok $m<a>:exists, 'an alias on a skipped plain group is still published';
    is ~$m<a>, '', 'as an empty Match';
    $m = "1" ~~ /$<x>=<[cd]>? 1/;
    ok $m<x>:exists, 'an alias on a skipped char class is still published';
    is ~$m<x>, '', 'as an empty Match';
}
{
    my $m = "xx" ~~ /<after x>* x/;
    is $m<after>.elems, 0, 'a zero-iteration <after x>* is an empty list, not a Match';
}
