use Test;

# `@@items` is list context applied to `@items`; it must consume both sigils
# rather than parsing the first as an anonymous empty array. Found in PURL.
plan 3;

my @items = <alpha beta>;

is-deeply @@items, [<alpha beta>], 'double array sigil reads the array values';
is +@@items, 2, 'double array sigil preserves the element count';
is @@items.join('|'), 'alpha|beta', 'double array sigil composes with methods';

# vim: expandtab shiftwidth=4
