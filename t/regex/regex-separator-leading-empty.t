use Test;

plan 14;

# A separated quantifier `<atom>+ % sep` whose atom can match the empty string
# must treat a LEADING empty element as a genuine element, matching Rakudo.
# (Regression: the separator matcher's zero-progress guard rejected a
# zero-width first atom, so `;b;c` failed to match while `a;;c`/`a;b;` worked.
# Both the ratcheted `token`/`rule` path and the backtracking `regex` path.)

grammar T {
    token plus   { <val>+ % ';' }
    token star   { <val>* % ';' }
    regex rgxpl  { <val>+ % ';' }
    token val    { <-[;]>* }
}

sub vals($rule, $s) {
    my $m = T.subparse($s, :rule($rule));
    $m.defined ?? $m<val>».Str.join('|') !! 'NOMATCH';
}

# Leading empty element (the regressed case).
is vals('plus', ';b;c'), '|b|c',   'leading empty element (+)';
is vals('plus', ';;'),   '||',     'all-empty elements (+)';
is vals('star', ';a'),   '|a',     'leading empty element (*)';

# Interior / trailing empties (already worked) still work.
is vals('plus', 'a;;c'), 'a||c',   'interior empty element';
is vals('plus', 'a;b;'), 'a|b|',   'trailing empty element';
is vals('plus', 'a;b;c'), 'a|b|c', 'no empty elements';

# Single empty element / empty input.
is vals('plus', ''),  '',   'empty input yields one empty element (+)';
is vals('star', ''),  '',   'empty input yields one empty element (*)';
is vals('plus', ';'), '|',  'one separator yields two empty elements';

# The end position advances past the whole run.
is T.subparse(';b;c', :rule<plus>).to, 4, 'match consumes the full leading-empty run';

# The non-ratcheted `regex` path behaves identically.
is vals('rgxpl', ';b;c'), '|b|c', 'leading empty element (regex, non-ratchet)';
is vals('rgxpl', ';;'),   '||',   'all-empty elements (regex, non-ratchet)';

# An atom that CANNOT match empty still yields zero elements on a leading sep.
grammar U {
    token list { <d>+ % ',' }
    token d    { \d+ }
}
nok U.subparse(',5', :rule<list>).defined && U.subparse(',5', :rule<list>).from == 0
    && U.subparse(',5', :rule<list>).to > 0,
    'non-empty-matching atom does not fabricate a leading empty element';
is U.subparse('5,6', :rule<list>)<d>».Str.join('|'), '5|6', 'non-empty atom list still works';
