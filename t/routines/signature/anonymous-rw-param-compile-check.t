use v6;
use Test;

plan 9;

# `is rw` on an anonymous parameter is rejected only when the parameter is
# optional (a default, `?`, or a named parameter without `!`); the check is made
# where the parameter is parsed, not by scanning the source text.
is EVAL(q[q{price: $ is rw}]), 'price: $ is rw', 'the text `$ is rw` inside a string is just text';
is EVAL(q[my $s = 'a $ is rw b'; $s]), 'a $ is rw b', 'in a single-quoted string';
is EVAL(q[# $ is rw in a comment
 42]), 42, 'in a comment';

lives-ok { EVAL 'sub f($ is rw) { }' }, 'a mandatory anonymous scalar may be rw';
lives-ok { EVAL 'sub g(Int $, $ is rw) { }' }, 'also after another parameter';

throws-like { EVAL 'sub h($ is rw = 1) { }' }, X::Trait::Invalid,
    :type<is>, :subtype<rw>, :declaring('an anonymous optional parameter'),
    'a default makes it optional';
throws-like { EVAL 'sub m(:$ is rw) { }' }, X::Trait::Invalid, 'an optional named anonymous parameter';
throws-like { EVAL 'sub foo(+$x [$ is rw = False]) { $x }' }, X::Trait::Invalid,
    'a sub-signature parameter is checked too';
throws-like { EVAL 'sub k($x? is rw) { }' }, X::Trait::Invalid, :name<$x>, 'a named optional one still is';
