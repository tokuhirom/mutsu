unit class ComputedVerAdverb:ver($?DISTRIBUTION.meta<ver>):auth(join ':', 'zef', 'me'):api(1 + 2);

# The `unit class` form of the computed declarator adverb, shaped after
# `unit class DBDish::SQLite:ver($?DISTRIBUTION.meta<ver>):api(...):auth(...)`.
# With no distribution behind `-I`, the `:ver` expression is undefined, which
# must still yield a defined (part-less) Version rather than a Version built
# from the source text.

method marker() { 'ok' }
