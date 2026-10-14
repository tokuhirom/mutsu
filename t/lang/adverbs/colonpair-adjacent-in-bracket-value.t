use v6;
use Test;

# Found via CSS::Specification t/00basic.t: `:required[ :seq[...] :rule<length>, ]`
# lost its second pair, because the items of a `:name[ ... ]` value were parsed
# with `expression`, which reads an adjacent `:b` as an adverb on `:a`.

plan 6;

is (:r[ :s[ 1, 2 ] :rule<l>, ]).raku, ':r([:s([1, 2]), :rule("l")])',
    'adjacent colonpairs inside :name[ ... ] are both kept';
is (:r[ :s(1) :t(2) ]).raku, ':r([:s(1), :t(2)])',
    'no trailing comma needed';
is (:r[ :s[:k<b>, :k<t>] :rule<l>, ]).value.elems, 2,
    'nested pair-list value keeps both items';
is (:r[ :a(1), :b(2) ]).value.elems, 2, 'comma-separated pairs still work';
is (:r[ 1, 2 ]).value.elems, 2, 'plain items still work';

my %h = :x[ :a(1) :b(2) ], :y(3);
is %h<x>.elems, 2, 'inside a hash literal / list';
