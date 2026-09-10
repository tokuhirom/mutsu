use v6;
use Test;

plan 7;

# In `[ A || B { code } ]` the second branch is never reached when A matches,
# so its code block must not run. The matcher evaluates every branch eagerly
# (a later branch's candidate ends are what let an enclosing pattern
# backtrack), which used to fire those side effects.

my @fired;

grammar Literal {
    token TOP { 'a' [ 'b' || . { @fired.push('literal') } ] }
}
@fired = ();
ok Literal.parse('ab'), 'literal first branch matches';
is-deeply @fired, [], 'losing branch block does not run (literal first branch)';

grammar Subrule {
    token TOP { 'a' [ <b> || . { @fired.push('subrule') } ] }
    token b { 'b' }
}
@fired = ();
ok Subrule.parse('ab'), 'subrule first branch matches';
is-deeply @fired, [], 'losing branch block does not run (subrule first branch)';

# The block still runs when its branch is the one that actually matches.
grammar Reached {
    token TOP { 'a' [ 'b' || . { @fired.push('reached') } ] }
}
@fired = ();
ok Reached.parse('az'), 'second branch matches when the first cannot';
is-deeply @fired, ['reached'], 'the winning branch block does run';

# The shape that motivated this: a losing branch whose block is a `die`.
# `Config::TOML::Parser::Grammar` spells its escape-sequence token this way,
# so every TOML document containing an escape died.
grammar Escapes {
    token TOP { <esc>+ }
    token esc { \\ [ <escape> || . { die "bad escape sequence" } ] }
    proto token escape {*}
    token escape:sym<n> { <sym> }
    token escape:sym<quote> { \" }
    token escape:sym<backslash> { \\ }
}
lives-ok { Escapes.parse('\\n\\"\\\\') or die 'no match' },
    'a die in the losing branch does not fire when <escape> matched';

done-testing;
