use v6;
use Test;

plan 13;

# `||` is ORDERED: raku enters branch n+1 only after branch n has been entered
# and the rest of the pattern has rejected every way it could match. A block in
# a later branch therefore runs exactly when the cursor reaches it — which the
# eager "measure every branch up front" matcher could not express. Every
# expectation below was measured against `raku`.

# The headline shape: branch 0 matches, but the atom AFTER the alternation
# cannot continue from its end, so the match backtracks into branch 1 and
# succeeds there. raku runs branch 1's block; mutsu used to skip it.
{
    my @fired;
    grammar G1 {
        regex TOP { 'a' [ 'bc' || 'b' { @fired.push('second') } ] 'cd' }
    }
    ok G1.parse('abcd').defined, 'backtracking into the second branch still matches';
    is-deeply @fired, ['second'], 'the second branch block runs when the cursor reaches it';
}

# The mirror case: branch 0 leads straight to a complete match, so branch 1 is
# never entered.
{
    my @fired;
    grammar G2 {
        regex TOP { 'a' [ 'b' { @fired.push('one') } || 'bc' { @fired.push('two') } ] 'cd' }
    }
    ok G2.parse('abcd').defined, 'first branch completes the match';
    is-deeply @fired, ['one'], 'the unreached branch block does not run';
}

# Branch 0 is entered (its block runs) and then rejected by the continuation,
# so branch 1 is entered too. raku fires BOTH.
{
    my @fired;
    grammar G3 {
        regex TOP { 'a' [ 'b' { @fired.push('one') } || 'bc' { @fired.push('two') } ] 'd' }
    }
    ok G3.parse('abcd').defined, 'second branch matches after the first is rejected';
    is-deeply @fired, ['one', 'two'], 'both entered branches run their blocks, in order';
}

# Three branches: the cursor walks them in written order until one sticks.
{
    my @fired;
    grammar G4 {
        regex TOP {
            'a' [ 'bcd' { @fired.push(1) } || 'bc' { @fired.push(2) } || 'b' { @fired.push(3) } ] 'cd'
        }
    }
    ok G4.parse('abcd').defined, 'third branch completes the match';
    is-deeply @fired, [1, 2, 3], 'every entered branch runs its block, in written order';
}

# Same rule outside a grammar.
{
    my @fired;
    ok ('abcd' ~~ / 'a' [ 'bc' || 'b' { @fired.push('second') } ] 'cd' /).defined,
        'plain regex backtracks into the second branch';
    is-deeply @fired, ['second'], 'plain regex runs the reached branch block';
}

# `:ratchet` (i.e. `token` / `rule`) commits to the first branch that matches,
# so a later branch is never entered at all — including when the ordered
# alternation sits inside a `|` alternative, which used to lose the ratchet.
{
    my @fired;
    grammar G5 {
        token TOP { 'z' | 'a' [ 'b' || . { @fired.push('loser') } ] }
    }
    ok G5.parse('ab').defined, 'ratcheted alternative matches';
    is-deeply @fired, [], 'a ratcheted losing branch is never entered';
}

# The ratchet that the `|` re-parse used to drop: a `token` is possessive, so
# `\d+` never gives a character back to the following `\d`.
{
    grammar G6 { token TOP { 'z' | \d+ \d } }
    nok G6.parse('12').defined, ':ratchet reaches a top-level | alternative';
}

done-testing;
