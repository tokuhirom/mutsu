use Test;

# A `%`/`%%` separated quantifier must backtrack the separator (and an
# optional trailing separator) against an outer anchor, while preserving the
# per-iteration capture structure. Previously a frugal separator (`(.+?)`) or a
# sequential-alternation atom (`||`) was routed to a capture-renumbering string
# expansion that could not admit more atoms.

plan 11;

# https://github.com/Raku/old-issue-tracker/issues/4279
{
    my $m = 'rule1 foo rule2 bar' ~~ /^ ( 'rule1' || 'rule2' )* %% (.+?) $/;
    ok $m, 'frugal-separator backtracking matches';
    is $m[0].elems, 2, 'atom group keeps both captures';
    is $m[1].elems, 2, 'separator group keeps both captures';
    is $m[0].map(~*).join(','), 'rule1,rule2', 'atom captures correct';
    is $m[1].map(~*).join('|'), ' foo | bar',  'separator captures correct';
}

# Plain literal separator still folds per-iteration captures.
{
    my $m = 'a,b,c' ~~ /^ (\w)+ %% (',') $/;
    is $m[0].map(~*).join(','), 'a,b,c', 'literal separator atom captures';
    is $m[1].elems, 2, 'literal separator captures (no trailing)';
}

# `%%` greedily consumes a trailing separator.
{
    my $m = 'a,b,c,' ~~ /^ (\w)+ %% (',') $/;
    ok $m, '%% with trailing separator matches';
    is $m[1].elems, 3, 'trailing separator captured';
}

# `%` (no trailing) does not consume a trailing separator.
{
    my $m = 'a,b,c' ~~ /^ (\w)+ % (',') $/;
    is $m[0].elems, 3, '% atom captures';
    is $m[1].elems, 2, '% separator captures';
}
