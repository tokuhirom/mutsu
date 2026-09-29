use Test;

# #9807: two symptoms of the same area -- what an attribute READ returns (the
# Scalar container or the decontainerized value).
#
# `self.x` calls the GENERATED accessor for `has $.x`, which decontainerizes
# its result -- raku: "there is a difference between self.a and $.a, since
# the latter will itemize; $.a will be equivalent to self.a.item or $(self.a)"
# (Language/objects.rakudoc). A `List` default assigned to a `$` attribute is
# itemized at construction like any other `$`-scalar store
# (attr-default-construction-itemizes.t), so `self.x` must decontainerize it
# back to a flattening List while `$.x` re-itemizes on top of that.
#
# `$!x`, read directly (not through the generated accessor), is unaffected:
# it preserves whatever item-ness the store put there -- so a constructor-set
# `has $.x` holding an Array is still itemized when a hand-written method
# returns `$!x`.
plan 7;

class A {
    has $.x = (1, 2, 3);
    method self_x_list { my @out; @out.push($_) for self.x; @out }
    method dollar_x_list { my @out; @out.push($_) for $.x; @out }
}
my $a = A.new;
is $a.self_x_list.elems, 3,
    'self.x is not itemized: for flattens the List default (3 iterations)';
is $a.dollar_x_list.elems, 1,
    '$.x is itemized: for sees it as one item';
is $a.dollar_x_list[0].raku, '$(1, 2, 3)',
    '$.x wraps the whole List as a single element';

class H {
    has $.x;
    method y { $!x }
}
is H.new(x => []).x.raku, '[]',
    'the generated accessor (.x) decontainerizes an Array default';
is H.new(x => []).y.raku, '$[]',
    '$!x preserves the constructor-provided Array\'s item container';

my $h = H.new(x => [1, 2]);
my @seen;
@seen.push($_) for $h.y;
is @seen.elems, 1,
    'for over $!x (holding an itemized Array) runs once';
is @seen[0].raku, '$[1, 2]',
    '... yielding the whole Array as the single item';
