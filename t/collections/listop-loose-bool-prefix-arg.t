use Test;

# HTTP::Message::Strict uses `grep so *, @lines.map: -> ...` while parsing
# chunked content. The loose prefix begins grep's first argument; its comma
# must remain available to the enclosing listop.
plan 4;

is-deeply (grep so *, 0, 1, 2).List, (1, 2), 'so WhateverCode starts a grep argument';
is-deeply (grep not *, 0, 1, 2).List, (0,), 'not WhateverCode starts a grep argument';
is-deeply (grep so *, (0, 1, 2).map: -> $x { $x }).List,
    (1, 2), 'a colon method call remains the second argument';
is-deeply (grep so * > 1, 0, 1, 2).List,
    (2,), 'a loose prefix consumes the comparison in its argument';
