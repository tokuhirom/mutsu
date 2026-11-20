use Test;

# The Match `.parse` returns carries the parse-level state on its own capture
# node instead of being rebuilt as an eager copy (#18207): `.actions`, the
# start rule's `.made`, the winning proto variant, aliases and `$<name>`.

plan 16;

grammar G {
    token TOP { '/' <word> [ '/' <word> ]* }
    token word { <[a..z]>+ }
}
class A {
    method word($/) { make ~$/ }
    method TOP($/) { make $<word>».made.join('|') }
}

{
    my $m = G.parse('/ab/cd', :actions(A));
    ok $m, 'parse with actions matches';
    is $m.made, 'ab|cd', 'start rule .made is set by its action';
    is $m.ast, 'ab|cd', '.ast reads the same value';
    ok $m.actions === A, '.actions is the actions object';
    is $m<word>[1].made, 'cd', 'child .made is reachable';
    is ~$m, '/ab/cd', 'the Match stringifies to the whole text';
    is ~$<word>[0], 'ab', '$<word> is set after the parse';
}

{
    my $m = G.parse('/ab');
    nok $m.actions.defined, 'no actions: .actions is undefined';
    nok $m.made.defined, 'no actions: .made is undefined';
}

grammar P {
    proto token item {*}
    token item:sym<num> { \d+ }
    token item:sym<word> { <[a..z]>+ }
}
class PA {
    method item:sym<num>($/) { make +$/ * 2 }
    method item:sym<word>($/) { make $/.uc }
}
{
    my $m = P.parse('21', :rule<item>, :actions(PA));
    is $m.made, 42, 'a proto start rule dispatches its :sym variant action';
    my $w = P.parse('hi', :rule<item>, :actions(PA));
    is $w.made, 'HI', 'the other variant too';
}

grammar Al {
    token TOP { <first=word> ' ' <rest=word> }
    token word { \w+ }
}
class AlA {
    method word($/) { make $/.flip }
    method TOP($/) { make $<first>.made ~ '-' ~ $<rest>.made }
}
{
    my $m = Al.parse('abc def', :actions(AlA));
    is $m.made, 'cba-fed', 'an aliased subrule runs its rule action';
    is ~$m<first>, 'abc', 'the alias names the capture';
}

{
    # An action object with state: the one passed is the one the Match keeps.
    class Counter { has $.n = 0; method word($/) { $!n++ } }
    my $c = Counter.new;
    my $m = G.parse('/a/b/c', :actions($c));
    is $m.actions.n, 3, '.actions is the very instance the actions ran on';
    # `.hash`/`.list` still read as a Match's.
    is $m.hash.keys.sort.join(','), 'word', '.hash keys';
    is $m<word>.elems, 3, 'quantified named capture has every match';
}
