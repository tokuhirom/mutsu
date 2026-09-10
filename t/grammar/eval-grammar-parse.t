use Test;

plan 6;

# A grammar declared inside EVAL'd code registers its class *and* its tokens.
# The tokens must survive the EVAL's routine-registry restore, or a later
# `.parse` on the returned grammar type object fails with "Unknown method parse".

# Named grammar via EVAL.
{
    my \g = EVAL 'grammar G_Named { token TOP { <a> | <b> }; token a { \d }; token b { <[a..z]> } }';
    is g.^name, 'G_Named', 'EVAL of a named grammar returns the grammar type object';
    ok g.parse('x').defined, '.parse works on an EVAL-defined named grammar';
    is g.parse('7')<a>, '7', 'named token matches through EVAL-defined grammar';
    nok g.parse('1x'), '.parse fails to match when the whole string is not consumed';
}

# Anonymous grammar via EVAL (fresh class each time).
{
    my \g = EVAL 'grammar { token TOP { <a> | <b> }; token a { \d | \s }; token b { a | b } }';
    ok g.parse('b').defined, '.parse works on an EVAL-defined anonymous grammar';
}

# Repeated EVAL of fresh grammars parsed in parallel must not crash
# (rakudo old-issue-tracker #2593: NFA-cache SEGV on first parallel parse).
{
    lives-ok {
        for ^20 {
            my \g = EVAL 'grammar { token TOP { <a> | <b> }; token a { \d | \s }; token b { a | b } }';
            await (start g.parse('b')) xx 4;
        }
    }, 'parallel parsing of freshly EVAL-defined grammars does not crash';
}
