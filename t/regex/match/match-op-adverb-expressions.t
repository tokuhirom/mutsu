use Test;

# `m//` evaluates its `:nth` and `:x` arguments as expressions where the match
# runs, like the `.match` method's named arguments (#18833).

plan 21;

my $n = 2;

is ~("abab" ~~ m:nth($n)/b/).from, "3", ':nth($n)';
is ~("abab" ~~ m:nth({2})/b/).from, "3", ':nth({2}) calls a block';
is ~("abab" ~~ m:nth(*)/b/).from, "3", ':nth(*) is the last match';
is ~("abab" ~~ m:2nd/b/).from, "3", ':2nd';
is ("abab" ~~ m:x($n)/b/).elems, 2, ':x($n)';
is ("abab" ~~ m:x(1..5)/b/).elems, 2, ':x(Range) takes up to its upper bound';
is ("abab" ~~ m:x(3)/b/).elems, 0, ':x(N) with fewer matches is an empty List';
is ("abab" ~~ m:g:x(1)/b/).elems, 1, ':g:x(1)';
is ("aaaa" ~~ m:ex:x(2)/a+/).map(~*).join(","), "aaaa,aaa", ':ex:x(2)';

{
    my @list = 1, 2;
    is ("abab" ~~ m:nth(@list)/b/).elems, 2, ':nth(@list)';
}
is ("abab" ~~ m:nth(1,5)/b/).elems, 1, ':nth(List) ignores indices past the last match';
is ("abab" ~~ m:nth(5,6)/b/).elems, 0, '... and is an empty List when none is left';
nok "abab" ~~ m:nth(5)/b/, ':nth(N) past the last match fails';
nok "abab" ~~ m:nth(*-5)/b/, 'a WhateverCode before the first match fails';
nok $/.defined, '... and leaves $/ Nil';

{
    ok "abab" ~~ m:nth(1|3)/b/, 'a Junction :nth succeeds when any eigenstate does';
    is $/.^name, 'Junction', '... and $/ is a Junction';
}

{
    my @froms;
    for 1..2 -> $i {
        @froms.push: ("abab" ~~ m:nth($i)/b/).from;
    }
    is-deeply @froms, [1, 3], 'the argument is evaluated at every match';
}

{
    given "abab" {
        is m:x(2)/b/.elems, 2, 'bare m:x(2)// against the topic';
    }
}

throws-like { "abab" ~~ m:nth(0)/b/ }, Exception,
    message => 'Attempt to retrieve before :1st match -- :nth(0)',
    ':nth(0) is before the first match';

is EVAL('"abab" ~~ m:nth($n)/b/').from, 3, 'EVAL sees the outer $n';
