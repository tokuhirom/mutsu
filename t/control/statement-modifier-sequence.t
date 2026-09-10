use Test;

plan 2;

my @generated;
@generated.push($_) for "foo", { $_ ~ "x" } ... "fooxx";
is-deeply @generated, ["foo", "foox", "fooxx"],
    'statement-modifier for absorbs sequence seeds before the ellipsis';

my @plain;
@plain.push($_) for 1, 2, 3;
is-deeply @plain, [1, 2, 3],
    'statement-modifier for still parses an ordinary comma list';
