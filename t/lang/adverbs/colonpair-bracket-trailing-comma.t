use v6;
use Test;

# A colonpair with a bracketed value and a trailing comma, `:name[X,]`, is an
# itemized 1-element list: the single element is NOT flattened in list context,
# so a single-pair Hash element stays a Hash (matching a plain `[X,]` literal).
# Previously the colonpair-bracket parser dropped the trailing comma and always
# emitted a flattening array, so `:depends[{:any["a","b"]},]` collapsed the Hash
# element into a Pair. (zef distribution-depends-parsing test 27:
# Zef::Distribution::DependencySpecification.new got a Pair instead of a Hash.)

plan 9;

my $a = :depends[{:a(1)},];       # trailing comma -> itemized, keep Hash
is $a.value.elems, 1,             'trailing comma: 1 element';
is $a.value[0].^name, 'Hash',     'trailing comma: element is a Hash';

my $b = :depends[{:a(1)}];        # no trailing comma -> flatten single Hash to Pair
is $b.value[0].^name, 'Pair',     'no trailing comma: single Hash flattens to Pair';

my $c = :depends[{:a(1)}, {:b(2)}];  # multiple elements -> each kept
is $c.value.elems, 2,             'two elements: 2 elements';
is $c.value[0].^name, 'Hash',     'two elements: first is a Hash';
is $c.value[1].^name, 'Hash',     'two elements: second is a Hash';

# The fatarrow form has always kept the Hash; the colonpair form must match it.
my $d = (depends => [{:a(1)},]);
is $d.value[0].^name, 'Hash',     'fatarrow form keeps Hash (baseline)';

# A scalar (non-pair) element with a trailing comma also stays itemized.
my @arr = <a b c>;
my $e = :list[@arr,];
is $e.value.elems, 1,             'trailing comma: array kept as single itemized element';
is $e.value[0].elems, 3,          'trailing comma: itemized array has its 3 elements';
