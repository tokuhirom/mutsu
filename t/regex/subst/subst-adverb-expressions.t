use Test;

# `s///` and `S///` evaluate their `:nth` and `:x` arguments as expressions,
# like the `.subst` method's named arguments (#18816).

plan 22;

my $n = 2;

{
    my $s = "aabbb";
    $s ~~ s:nth($n)/<[abc]>//;
    is $s, "abbb", ':nth($n) takes a variable';
}

{
    my $s = "aabbb";
    $s ~~ s:nth({2})/<[abc]>//;
    is $s, "abbb", ':nth({2}) calls a block';
}

{
    my $s = "aabbb";
    $s ~~ s:nth(*-1)/<[abc]>/X/;
    is $s, "aabXb", ':nth(*-1) is the second-to-last match';
    isa-ok $/, Match, 'a WhateverCode :nth leaves a single Match';
}

{
    my $s = "aabbb";
    $s ~~ s:nth(1,3)/<[abc]>/X/;
    is $s, "XaXbb", ':nth(1,3) selects two matches';
    is $/.elems, 2, '... and $/ is a List of them';
}

{
    my $s = "aaa";
    $s ~~ s:nth(1..2)/a/b/;
    is $s, "bba", ':nth(1..2) selects a span';
    is $/.elems, 2, '... and $/ is a List';
}

{
    my $s = "aaa";
    $s ~~ s:nth(Inf)/a/b/;
    is $s, "aab", ':nth(Inf) is the last match';
}

{
    my $s = "aaa";
    $s ~~ s:nth(*-5)/a/b/;
    is $s, "aaa", 'a WhateverCode :nth before the first match selects nothing';
    nok $/.defined, '... and $/ is Nil';
}

{
    my $s = "aabbb";
    $s ~~ s:x($n)/<[abc]>/X/;
    is $s, "XXbbb", ':x($n) takes a variable';
}

{
    my $s = "aaa";
    $s ~~ s:x(5)/a/b/;
    is $s, "aaa", ':x(N) with fewer matches replaces nothing';
}

{
    my @list = 1, 3;
    my $s = "aaaa";
    $s ~~ s:nth(@list)/a/b/;
    is $s, "baba", ':nth(@list) takes an array';
}

is (S:x(1..2)/b/Y/ given "abbb"), "aYYb", 'S:x(Range)';
is (S:nth($n)/b/Z/ given "abab"), "abaZ", 'S:nth($n)';

{
    $_ = "aaa";
    s:nth(1,3):x(1)/a/Q/;
    is $_, "Qaa", ':nth and :x combine';
}

{
    my $s = "a1b2c3";
    $s ~~ s:nth($n)[\d] = "<{$/ * 10}>";
    is $s, "a1b<20>c3", 's[...] = EXPR with :nth($n)';
}

throws-like { $_ = "aaa"; s:nth(0)/a/b/ }, Exception,
    message => 'Attempt to retrieve before :1st match -- :nth(0)',
    ':nth(0) is before the first match';
throws-like { $_ = "aaa"; s:nth(3,1)/a/b/ }, Exception,
    message => 'Attempt to fetch match #1 after #3',
    ':nth must be ascending';
throws-like { "aaa".match(/a/, :nth(0)) }, Exception,
    message => 'Attempt to retrieve before :1st match -- :nth(0)',
    '.match reports :nth(0) the same way';
throws-like { "aaa".match(/a/, :nth(-Inf)) }, Exception,
    message => 'Attempt to retrieve before :1st match -- :nth(-Inf)',
    '.match names -Inf as written';
