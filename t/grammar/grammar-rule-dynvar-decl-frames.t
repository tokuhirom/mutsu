# ADR-0135 Slice E: a grammar whose rules declare `:my $*x` runs on the
# compiled regex engine. Each rule invocation is a frame whose window holds
# its own declarations: initialized when the call is made, removed when the
# callee returns or fails, and installed again when backtracking resumes
# inside the callee. Values are rakudo's.
use Test;

plan 8;

# A ratcheted callee commits to its first end, as it does without any
# declaration in the grammar.
{
    my @log;
    grammar Commit {
        token TOP { :my $*D = 1; <a> 'x' }
        token a { <b> { @log.push: "a:" ~ $/.to } }
        token b { \w+ { @log.push: "b:" ~ $/.to } | \w { @log.push: 'b1' } }
    }
    nok Commit.parse('abx'), 'a ratcheted callee is not re-entered for a shorter end';
    is @log.join(','), 'b:3,a:3', 'only the first end of the ratcheted callee ran';
}

# A declaration is visible to the rules the declaring rule calls, and ends
# with the declaring rule.
{
    my @seen;
    grammar Scope {
        token TOP { <outer> <after> }
        token outer { :my $*LEVEL = 'outer'; <inner> }
        token inner { . { @seen.push: $*LEVEL } }
        token after { . { @seen.push: $*LEVEL // 'none' } }
    }
    ok Scope.parse('ab'), 'parses';
    is @seen.join(','), 'outer,none', 'the declaration is live only inside its rule';
}

# Nested declarations of the same name shadow and restore.
{
    my @seen;
    grammar Nest {
        token TOP { :my $*N = 'top'; <r> <a> <r> }
        token a { :my $*N = 'a'; <r> }
        token r { . { @seen.push: $*N } }
    }
    ok Nest.parse('abc'), 'parses';
    is @seen.join(','), 'top,a,top', 'an inner declaration shadows the outer one only while it runs';
}

# Backtracking into a non-ratchet callee after it returned re-installs its
# declaration for the code that decides the new end.
{
    grammar Back {
        regex TOP { <w> 'c' }
        regex w { :my $*WANT = 'ab'; (\w+) <?{ ~$0 eq $*WANT }> }
    }
    is ~Back.parse('abc')<w>, 'ab', 'the declaration is live again when backtracking into the callee';
}

# A rule's action reads the value its declaration held when the rule matched.
{
    grammar Act {
        token TOP { <item> }
        token item { :my $*K = 'start'; \w+ { $*K = 'seen ' ~ $/ } }
    }
    class ActA {
        method item($/) { make $*K }
        method TOP($/)  { make $<item>.made }
    }
    is Act.parse('xy', :actions(ActA)).made, 'seen xy', 'the action sees the final value';
}
