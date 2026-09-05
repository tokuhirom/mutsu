use Test;

# An embedded `{ … }` block in a grammar rule runs INLINE, where the cursor
# reaches it — not at reduce time. A `make`-bearing block used to be deferred to
# the post-match bottom-up walk, which visits children before parents, so a
# rule's own block ran after its subrules' blocks, and a later block of the same
# rule could not see what an earlier one had `make`d.
#
# Every expectation below was verified against `raku`.

plan 14;

# --- The headline case: the grammar_tutorial.rakudoc line-679 example. --------

{
    my @log;
    grammar G {
        rule TOP { <function-define> }
        rule function-define {
            'sub' <identifier>
            {
                @log.push("func " ~ $<identifier>.made);
                make $<identifier>.made;
            }
            '(' <parameter> ')' '{' '}'
            { @log.push("end " ~ $/.made); }
        }
        token identifier { \w+ { make ~$/; } }
        token parameter { \w+ { @log.push("param " ~ $/); } }
    }
    ok G.parse('sub f ( a ) { }').defined, 'the tutorial grammar parses';
    is-deeply @log, ['func f', 'param a', 'end f'],
        'blocks run in match order and the first block\'s make reaches the second';
}

# --- Variant 1: no `make` anywhere. Already correct before the fix; pinned so
# --- the always-inline path cannot regress it.
{
    my @log;
    grammar G1 {
        rule TOP { <function-define> }
        rule function-define {
            'sub' <identifier>
            { @log.push('block1') }
            '(' <parameter> ')' '{' '}'
            { @log.push('block2') }
        }
        token identifier { \w+ { @log.push('identifier') } }
        token parameter { \w+ { @log.push('parameter') } }
    }
    ok G1.parse('sub f ( a ) { }').defined, 'side-effect-only grammar parses';
    is-deeply @log, ['identifier', 'block1', 'parameter', 'block2'],
        'side-effect-only blocks keep match order';
}

# --- Variant 2: `make` in BOTH of the rule's blocks. -------------------------
{
    my @log;
    grammar G2 {
        rule TOP { <function-define> }
        rule function-define {
            'sub' <identifier>
            { @log.push('block1 ' ~ $<identifier>.made); make $<identifier>.made; }
            '(' <parameter> ')' '{' '}'
            { @log.push('block2 ' ~ $/.made); make $/.made; }
        }
        token identifier { \w+ { make ~$/; } }
        token parameter { \w+ { @log.push('parameter ' ~ $/); } }
    }
    my $m = G2.parse('sub f ( a ) { }');
    is-deeply @log, ['block1 f', 'parameter a', 'block2 f'],
        'two make-bearing blocks of one rule keep match order';
    is $m<function-define>.made, 'f', 'the last make of the rule wins';
}

# --- A later block of the same rule reads an earlier one's make via `$/.made`.
{
    my $seen;
    ok 'ab' ~~ / a { make 7 } b { $seen = $/.made } /, 'plain regex with two blocks matches';
    is $seen, 7, 'a later block sees the earlier block\'s make through $/.made';
    is $/.made, 7, 'the regex-level .made is the value that was made';
}

# --- A subrule's `make` is committed by the time the parent's next block runs.
{
    my @log;
    grammar F {
        token TOP { <a> { @log.push('p1 ' ~ $<a>.made) } <b> { @log.push('p2 ' ~ $<b>.made) } }
        token a { \d+ { make 'A' ~ $/ } }
        token b { \w+ { make 'B' ~ $/ } }
    }
    ok F.parse('12xy').defined, 'two-subrule grammar parses';
    is-deeply @log, ['p1 A12', 'p2 Bxy'],
        '$<child>.made is available to the parent block that follows the subrule';
}

# --- Blocks re-run on backtracking, and the winning path's make is the result.
{
    my @log;
    ok 'aaab' ~~ / (\w)+ { @log.push(+@log + 1); make +@log } b /,
        'backtracking pattern with a make-bearing block matches';
    is $/.made, 2, 'the block ran twice and the second (winning) make is the result';
}

# --- A `:my $*x` block still gets its per-match binding (it stays on the
# --- reduce-time path, which is what makes the per-match binding work).
{
    grammar D {
        token TOP { <part>+ % ',' }
        token part { :my $*V = 'decl'; \w+ [ <?before ','> { $*V = 'set' } ]? { make $*V } }
    }
    is D.parse('a,b').<part>.map(*.ast).join('|'), 'set|decl',
        'a dynamic-variable block still sees its own match binding';
}
