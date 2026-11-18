use Test;

# A multi sub whose dispatch fails raises X::Multi::NoMatch; it is not
# answered by a same-named token of a grammar that encloses the caller's
# package (CSS::Specification::Compiler::Grammars' `multi sub ws` under the
# grammar CSS::Specification, whose `token ws` came back as a Regex).

plan 2;

grammar Outer { token ws { x } }

package Outer::Inner {
    multi sub ws(Int $x) { "ws $x" }
    our sub ok-call { ws(1) }
    our sub bad-call($arg) { ws($arg) }
}

is Outer::Inner::ok-call(), 'ws 1', 'a matching candidate runs';
throws-like { Outer::Inner::bad-call('s') }, X::Multi::NoMatch,
    'no candidate is NoMatch, not the outer grammar token';
