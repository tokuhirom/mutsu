use Test;

# `.subst(/re/, { ... })` invokes the replacement block's compiled code for each
# match instead of compiling its AST once per match (#10120, ADR-0133). The
# block is called like Rakudo's `$replacement.count ?? $replacement($/) !!
# $replacement()`, with `$/`, `$0`.. and `$<name>` holding the current match.

plan 12;

is "abcab".subst(/(a)(b)/, { "[$0|$1|$/|$_]" }, :g), '[a|b|ab|ab]c[a|b|ab|ab]',
    'bare block sees $/, $0, $1 and $_ per match';

is "foo bar".subst(/$<w>=(\w+)/, { "<$<w>>" }, :g), '<foo> <bar>',
    'named capture $<w> per match';

{
    $_ = 'T';
    is "ab".subst(/a/, -> $m { "$m$_" }), 'aTb',
        'a pointy block binds the match to its parameter and keeps the outer $_';
}

is "ab".subst(/a/, -> { 'Y' }), 'Yb', 'a zero-arity pointy block is called with no argument';
is "abc".subst(/b/, *.uc), 'aBc', 'a WhateverCode replacement receives the match';
is "aaa".subst(/a/, { $^m ~ '!' }, :x(2)), 'a!a!a', 'a placeholder block receives the match';

{
    my $n = 0;
    "aaa".subst(/a/, { $n++; 'b' }, :g);
    is $n, 3, 'a closed-over counter keeps its writes across matches';
}

{
    sub f { my $k = 10; "aaa".subst(/a/, { $k++ }, :g) ~ $k }
    is f(), '10111213', 'writes to a routine lexical reach the routine';
}

{
    my @l;
    "a1b2".subst(/\d/, { @l.push: +$/; '' }, :g);
    is-deeply @l, [1, 2], 'the block can push to a captured array';
}

is "aXbX".subst(/X/, { .uc ~ "y".subst(/y/, { "Z$/" }) }, :g), 'aXZybXZy',
    'a nested .subst inside the replacement block';

{
    $_ = 'topic';
    "ab".subst(/a/, { 'X' });
    is $_, 'topic', 'the caller topic is untouched after the call';
}

throws-like { "abc".subst(/b/, { die "boom" }) }, Exception, message => 'boom',
    'an exception in the replacement block propagates';
