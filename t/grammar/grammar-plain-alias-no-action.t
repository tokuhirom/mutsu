use Test;

# Rakudo calls an action method when a SUBRULE reduces. An alias on anything
# else (`$<x>=(...)`, `$<x>=[...]`, `$<x>=<:!Cc>*`, `$<x>=<[a..z]>+`) names a
# capture, not a rule, so no `method x` runs for it. mutsu dispatched an action
# named after the alias, so Log::Reader's `method remark` ran twice: once for
# `token remark` and once for its own `$<remark> = <:!Cc>*` capture, where it
# read a missing `$<remark>` and warned "Use of Nil in string context".

plan 6;

my @calls;

grammar G {
    token TOP     { '#' [ ||<version> ||<remark> ] }
    token version { 'Version:' \s* $<ver>=(\d+) }
    token remark  { 'Remark:' $<remark> = <:!Cc>* }
}
class A {
    method ver($/)     { @calls.push: 'ver' }
    method version($/) { @calls.push: 'version:' ~ $<ver> }
    method remark($/)  { @calls.push: 'remark:' ~ $<remark> }
}

@calls = ();
G.parse('#Remark: 12', actions => A);
is-deeply @calls, ['remark: 12'], 'alias on a char class does not fire the same-named action';

@calls = ();
G.parse('#Version: 12', actions => A);
is-deeply @calls, ['version:12'], 'alias on a capture group does not fire an action';

grammar H {
    token TOP  { $<w>=[<word>] ' ' $<n>=(<num>) ' ' $<c>=<[a..z]>+ ' ' $<a>=<word> }
    token word { \w+ }
    token num  { \d+ }
}
class B {
    method w($/)    { @calls.push: 'w' }
    method n($/)    { @calls.push: 'n' }
    method c($/)    { @calls.push: 'c' }
    method a($/)    { @calls.push: 'a' }
    method word($/) { @calls.push: 'word:' ~ $/; make ~$/ }
    method num($/)  { @calls.push: 'num:' ~ $/; make +$/ }
}

@calls = ();
my $m = H.parse('foo 42 bar baz', actions => B);
is-deeply @calls, ['word:foo', 'num:42', 'word:baz'],
    'only subrule calls fire actions; aliases on groups and char classes do not';
is $m<word>[0].made, 'foo', 'subrule inside an aliased [ ] group still reduces';
is $m<n><num>.made, 42, 'subrule inside an aliased ( ) group still reduces';
is $m<a>.made, 'baz', 'an alias on a subrule call still dispatches the subrule action';
