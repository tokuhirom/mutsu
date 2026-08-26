use Test;

# A backreference (`$0` / `$<name>`) written inside an inline sub-pattern must
# still resolve against the captures the enclosing pattern level has already
# taken. mutsu matched every inline sub-pattern with its own fresh capture
# store, so `/ $<x>=(\w) [ $<x> ] /` never matched anything.
#
# The scope rules below are pinned against real raku: a non-capturing group,
# both alternation flavours, a conjunction and a `~` goal share the enclosing
# capture scope; a CAPTURING group and a lookaround get their own cursor and
# therefore do NOT see the outer captures (and hide them from anything nested
# deeper inside).
#
# Found while re-measuring the `XML` battery candidate: `XML::Grammar`'s
# `element` token closes with `[ '/>' | '>' <child>* '</' $<name> '>' ]`, so no
# XML document with a closing tag parsed at all.

plan 14;

ok 'aa' ~~ / $<x>=(\w) $<x> /,          'named backref, no group';
ok 'aa' ~~ / $<x>=(\w) [ $<x> ] /,      'named backref inside a non-capturing group';
ok 'aa' ~~ / (\w) $0 /,                 'positional backref, no group';
ok 'aa' ~~ / (\w) [ $0 ] /,             'positional backref inside a non-capturing group';
ok 'aa' ~~ / $<x>=(\w) [ [ $<x> ] ] /,  'backref two groups deep';
ok 'aa' ~~ / $<x>=(\w) [ 'Z' | $<x> ] /,   'backref in a `|` alternation branch';
ok 'aa' ~~ / $<x>=(\w) [ 'Z' || $<x> ] /,  'backref in a `||` alternation branch';
ok 'aaa' ~~ / $<x>=(\w) [ $<x> ]+ /,    'backref under a quantified group';
ok 'aa' ~~ / $<x>=(\w) [ $<x> && \w ] /,   'backref in a conjunction';
ok 'a(a)' ~~ / $<x>=(\w) '(' ~ ')' $<x> /, 'backref inside a `~` goal';

# Capture-scope barriers (all four of these FAIL under raku too).
nok 'aa' ~~ / $<x>=(\w) ( $<x> ) /,
    'a capturing group does not see the enclosing named capture';
nok 'aa' ~~ / (\w) ( $0 ) /,
    'a capturing group does not see the enclosing positional capture';
nok 'aa' ~~ / $<x>=(\w) ( [ $<x> ] ) /,
    'the capturing-group barrier also hides the outer level from a nested group';
nok 'aa' ~~ / $<x>=(\w) <?before $<x>> . /,
    'a lookaround does not see the enclosing capture';
