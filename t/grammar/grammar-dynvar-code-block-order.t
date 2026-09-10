use Test;

plan 12;

# An embedded `{ … }` block that mentions a `$*`/`@*`/`%*` dynamic variable runs
# INLINE, where the cursor reaches it, exactly like every other block. It used to
# be deferred to the post-match bottom-up reduce walk, so within one rule a
# dynamic-variable block ran AFTER the rule's plain blocks and after its
# subrules' blocks — two blocks of the same rule executing in reverse source
# order. Every expectation below was checked against real `raku` first.

{
    grammar G {
        token TOP { :my $*N = 0; <a> { $*N = 1; @*ORDER.push('dyn') } { @*ORDER.push('inline') } <b> }
        token a { \w }
        token b { \w }
    }
    my @*ORDER;
    ok G.parse('xy').defined, 'the mixed inline/dynamic-block rule parses';
    is @*ORDER.join(','), 'dyn,inline',
        'a $*-mentioning block runs in source order relative to a plain sibling';
}

{
    # A subrule's own block runs when the cursor passes the subrule, so a later
    # dynamic block of the parent sees it as already done.
    my @*SEQ;
    grammar G2 {
        token TOP { <a> { @*SEQ.push('parent') } }
        token a { \w { @*SEQ.push('child') } }
    }
    ok G2.parse('x').defined, 'the nested-block grammar parses';
    is @*SEQ.join(','), 'child,parent',
        'a subrule block precedes the parent block that follows the subrule';
}

# ---------------------------------------------------------------------------
# A rule's `:my $*x` is still one binding per match, and an inline write to it
# is visible both to a later block of the same match and to that match's action.
# ---------------------------------------------------------------------------

{
    grammar G3 {
        token TOP { <part>+ % ',' }
        token part { :my $*V = 'decl'; \w+ [ <?before ','> { $*V = 'set' } ]? { make $*V } }
    }
    is G3.parse('a,b').<part>.map(*.ast).join('|'), 'set|decl',
        'an inline block write to $*V reaches a later block of the same match';

    class A3 { method part($/) { make $*V } }
    is G3.parse('a,b', :actions(A3)).<part>.map(*.ast).join('|'), 'set|decl',
        'and the action of that match re-installs the same per-match value';
}

# ---------------------------------------------------------------------------
# A write an inline block makes to an in-regex lexical must escape the
# sub-pattern it was made in. The single-candidate matcher propagated
# `regex_vars` out of a group; the plural (candidate-list) matcher — the one the
# main walk actually uses — dropped them, so a write inside ANY `[ … ]`, `( … )`
# or alternation branch was silently lost.
# ---------------------------------------------------------------------------

{
    grammar Lex {
        token plain-group  { :my $V = 'decl'; \w [ { $V = 'set' } ] { make $V } }
        token capture-grp  { :my $V = 'decl'; \w ( { $V = 'set' } ) { make $V } }
        token seq-alt      { :my $V = 'decl'; \w [ { $V = 'set' } || { $V = 'other' } ] { make $V } }
        token nested       { :my $V = 'decl'; \w [ [ { $V = 'set' } ] ] { make $V } }
        token quantified   { :my $V = 0; [ \w { $V++ } ]+ { make $V } }
        token skipped      { :my $V = 'decl'; \w [ 'z' { $V = 'set' } ]? { make $V } }
    }
    is Lex.parse('x', :rule<plain-group>).ast, 'set',
        'a write inside a non-capturing group leaves the group';
    is Lex.parse('x', :rule<capture-grp>).ast, 'set',
        'a write inside a capturing group leaves the group';
    is Lex.parse('x', :rule<seq-alt>).ast, 'set',
        'a write inside an ordered-alternation branch leaves the branch';
    is Lex.parse('x', :rule<nested>).ast, 'set',
        'a write two groups deep still reaches the enclosing rule';
    is Lex.parse('xx', :rule<quantified>).ast, 2,
        'each iteration of a quantified group accumulates into the lexical';
    is Lex.parse('x', :rule<skipped>).ast, 'decl',
        'a group the cursor never entered leaves the declared value alone';
}
