use Test;

plan 7;

# A `$.attr` / `%.attr` access is `self.attr` in *item* / *hash* context — the
# sigil contextualizes the accessor result. `@.attr` keeps list context.
# Mirrors roast/S12-methods/accessors.t subtests 2, 4, 5.
class A {
    has @.a;
    has $.b;
    method item-a  { my $x = 0; $x++ for flat 0, $.a; $x - 1 }   # $.a as one item
    method list-a  { my $x = 0; $x++ for @.a;          $x     }   # @.a flattens
    method item-b  { my $x = 0; $x++ for flat 0, $.b;  $x - 1 }   # $.b as one item
    method hash-a  { my $x = 0; $x++ for %.a;          $x     }   # %.a as a hash
    method raku-a  { ($.a).raku }
}

my $o = A.new(a => (1, 2, 3, 4), b => [3, 4, 5, 6]);

is $o.list-a, 4, '@.a contextualizes as a flat list';
is $o.item-a, 1, '$.a contextualizes as a single item';
is $o.item-b, 1, '$.b (Array in a scalar attr) stays a single item';
is $o.hash-a, 2, '%.a contextualizes as a hash (2 key/value pairs)';
ok $o.raku-a.starts-with('$'), '$.a is itemized ($[...]) in .raku';

# Method calls and indexing still see through the itemization.
is $o.a.elems, 4, '@.a accessor still has 4 elements';
is ($o.a)[2], 3, 'indexing the accessor result works';
