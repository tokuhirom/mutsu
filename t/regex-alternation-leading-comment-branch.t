use v6;
use Test;

# ADR-0022 Slice 3 regression (roast/S05-grammar/signatures.t): a `|`
# alternation that opens with a leading `|` for visual alignment, where the
# text before that first `|` is a comment (not pure whitespace), used to
# parse into a real (but always zero-width-matching) empty `RegexPattern`
# branch. The old longest-actual-match ranking silently deprioritized it
# (0 < any real match length), but ADR-0022's declarative-prefix ranking can
# tie it with a real branch that also terminates its measurement early (e.g.
# a leading code block) and then prefer the phantom empty branch via the
# declaration-order tie-break, making the whole match spuriously zero-width.

"bar" ~~ /
    #a leading comment before the aligned pipe
    | 'bar'
    | 'baz'
/;
is ~$/, "bar", 'a comment before an aligned leading pipe does not create a phantom empty branch';

{
    my $arg = 2;
    my token fred($arg, $bar?) {    #OK not used
        | { $arg == 1 } 'bar'
        | { $arg == 2 } 'foo'
    }
    ok "foo" ~~ /<fred(2,3)>/, 'a comment right after the signature does not break code-gated LTM ranking';
}

{
    grammar G {
        token TOP {
            <fred(1)>
            <fred: 2, 3>
        }

        token fred($arg, $bar?) {    #OK not used
            | { $arg == 1 } 'bar'
            | { $arg == 2 } 'foo'
        }
    }
    ok G.parse("barfoo"), 'grammar token with a signature comment and code-gated alternation parses correctly';
}

# The `()`/`[]` group forms share the same leading-empty-branch elision logic
# (regex_branch_is_blank) — pin them too.
"bar" ~~ / ( #comment
    | 'bar' | 'baz' ) /;
is ~$/, "bar", 'a comment before an aligned leading `|` inside a capture group is elided too';

"bar" ~~ / [ #comment
    | 'bar' | 'baz' ] /;
is ~$/, "bar", 'a comment before an aligned leading `|` inside a non-capturing group is elided too';

# Operators inside regex comments are inert and must not become top-level
# alternation/conjunction separators.
"bar" ~~ / # a comment mentioning `|` and & operators
    | 'bar'
    | 'baz'
/;
is ~$/, "bar", 'operators in a line comment do not split alternation';

ok 'bar' ~~ / #`[an embedded comment containing | and &] 'bar' /,
    'operators in an embedded comment do not split alternation or conjunction';

done-testing;
