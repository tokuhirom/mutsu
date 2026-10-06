use Test;

# A `<name>` call whose rule has several `multi` candidates and NO proto is
# an ordinary multi dispatch over the candidates' signatures in rakudo: the
# narrowest signature wins, two equally narrow ones are ambiguous, and none
# accepting the arguments is a NoMatch. (A proto, or `:sym<>` variants, is
# what makes the candidates a longest-token alternation instead.) mutsu used to
# run every candidate and union the ends. Expected values come from `raku`.

plan 13;

# Identical signatures: ambiguous, whichever candidate would have matched.
{
    grammar Same {
        multi token t { a }
        multi token t { b }
        token TOP { <t> }
    }
    throws-like { Same.parse('a') }, X::Multi::Ambiguous, 'identical protoless multi candidates are ambiguous (first)';
    throws-like { Same.parse('b') }, X::Multi::Ambiguous, 'identical protoless multi candidates are ambiguous (second)';
    # The failure is not memoized into a quiet non-match.
    throws-like { Same.parse('b') }, X::Multi::Ambiguous, 'the ambiguity is raised again by the next parse';
}

# A branch the match never reaches does not raise.
{
    grammar Unreached {
        multi token t { a }
        multi token t { b }
        token TOP { x | <t> }
    }
    is Unreached.parse('x').Str, 'x', 'an unreached ambiguous call does not die';
}

# Distinct arities: the zero-argument candidate answers `<t>`, the other `<t(1)>`.
{
    grammar Arity {
        multi token t { a }
        multi token t($x) { b }
        token TOP { <t> }
        token ONE { <t(1)> }
    }
    is Arity.parse('a').Str, 'a', 'the zero-argument candidate answers an argument-less call';
    nok Arity.parse('b'), 'the one-argument candidate does not';
    is Arity.parse('b', :rule<ONE>).Str, 'b', 'the one-argument candidate answers <t(1)>';
}

# A narrower signature wins outright; the wider one does not run as well.
{
    grammar Narrow {
        multi token t(Int $x) { a }
        multi token t($x) { b }
        token TOP { <t(1)> }
        token STR { <t('s')> }
    }
    is Narrow.parse('a').Str, 'a', 'the Int candidate answers an Int argument';
    nok Narrow.parse('b'), 'the wider candidate is not also tried for an Int argument';
    is Narrow.parse('b', :rule<STR>).Str, 'b', 'the wider candidate answers a Str argument';
}

# No candidate accepts the call.
{
    grammar NoMatch {
        multi token t($x where 1) { a }
        multi token t($x) { b }
        token TOP { <t> }
    }
    throws-like { NoMatch.parse('b') }, X::Multi::NoMatch, 'an argument-less call no candidate accepts is NoMatch';
}

# A proto keeps the candidates a longest-token alternation.
{
    grammar Proto {
        proto token t {*}
        token t:sym<a> { a }
        token t:sym<ab> { ab }
        token TOP { <t> }
    }
    is Proto.parse('ab').Str, 'ab', 'a proto still ranks its :sym<> candidates by longest token';
}

# A single multi candidate is not a dispatch problem.
{
    grammar Single {
        multi token t { a }
        token TOP { <t> }
    }
    is Single.parse('a').Str, 'a', 'a lone multi candidate is called';
}
