use Test;

# Raku allows at most one statement modifier, except a *conditional* modifier
# (if/unless/with/without) immediately followed by a *loop* modifier
# (for/while/until/given) — `EXPR if COND for LIST`. Any other chain needs a `;`
# and is X::Syntax::Confused ("Missing semicolon").

plan 9;

throws-like 'say 1 if 2 if 3 { say 3 }', X::Syntax::Confused,
    'two conditional modifiers';
throws-like 'say 1 if 2 if 3', X::Syntax::Confused,
    'two conditional modifiers (no block)';
throws-like 'say 1 for 1..2 for 3..4', X::Syntax::Confused,
    'two loop modifiers';
throws-like 'say 1 for 3..4 if 2', X::Syntax::Confused,
    'loop then conditional';

# The one legal chain plus all single modifiers.
is-deeply (($_ * $_ if $_ %% 2 for 0..5)).list, (0, 4, 16),
    'conditional then loop chain (EXPR if COND for LIST)';
lives-ok { EVAL 'say 1 if 2' }, 'single if lives';
lives-ok { EVAL 'say $_ for 1..3' }, 'single for lives';
lives-ok { EVAL 'say 1 unless 0' }, 'single unless lives';
lives-ok { EVAL 'say 1 if 2 for 3..4' }, 'if then for lives';
