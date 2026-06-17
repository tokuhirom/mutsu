use Test;

# The reverse meta-operator `R` applied to a range operator (`R..`, `R^..`,
# `R..^`, `R^..^`) swaps the endpoints: `a R.. b` is `b .. a`. Such a range also
# carries the precedence worry: a bare `|`/`~` prefix means `|(a R.. b)`, so
# `use fatal; |4 R.. 5` must throw X::Worry::Precedence::Range, but a
# parenthesized form must not.

plan 13;

# reversed-range construction
is-deeply (4 R..  5), (5 ..  4), 'R.. swaps endpoints (inclusive)';
is-deeply (4 R^.. 5), (5 ^.. 4), 'R^.. swaps endpoints (exclusive start)';
is-deeply (4 R..^ 5), (5 ..^ 4), 'R..^ swaps endpoints (exclusive end)';
is-deeply (4 R^..^ 5), (5 ^..^ 4), 'R^..^ swaps endpoints (exclusive both)';

# a non-empty reversed range iterates
is-deeply (5 R.. 2).list, (2, 3, 4, 5), 'R.. produces an iterable range';

# the precedence worry fires (under use fatal) for a bare prefix on a reversed range
for «R.. R^.. R..^ R^..^» -> $op {
    throws-like "\{ use fatal; |4 $op 5 }", X::Worry::Precedence::Range,
        "$op warns on bare Slip-flatten prefix";
}

# parenthesized forms do not warn
eval-lives-ok '{ use fatal; |(4 R.. 5) }', 'parenthesized range does not warn (flatten)';
eval-lives-ok '{ use fatal; (|4) R.. 5 }', 'parenthesized endpoint does not warn (flatten)';
eval-lives-ok '{ use fatal; ~(4 R.. 5) }', 'parenthesized range does not warn (stringify)';
eval-lives-ok '{ use fatal; (~4) R.. 5 }', 'parenthesized endpoint does not warn (stringify)';
