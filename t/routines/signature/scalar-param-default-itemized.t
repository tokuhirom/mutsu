use Test;

# A `$` parameter's default value is an item binding, exactly like a supplied
# argument: `sub f($a = [])` binds `$[]`, so `%h{$a}` is one key, not a slice
# of zero keys. The same holds after a coercion (`List() :$array = []`).
# PDF's `PDF::COS::Array.new(List() :$array = [], |c)` looks up
# `%seen{$array}`, and the slice answered `()`, which `with` took for a found
# object (#12907). Expected values are the `raku` (v2026.09) output.

plan 6;

sub pos-default($a = []) { $a.raku }
is pos-default(), '$[]', 'a positional default Array is itemized';

sub named-default(:$a = [1, 2]) { $a.raku }
is named-default(), '$[1, 2]', 'a named default Array is itemized';

sub coerced-default(List() :$a = []) { $a.raku }
is coerced-default(), '$[]', 'a coerced named default stays itemized';

my %seen{Any};
sub lookup(List() :$array = []) {
    with %seen{$array} -> $o { 'found' } else { 'none' }
}
is lookup(), 'none', 'a defaulted Array subscripts an object hash as one key';

my %h;
sub key-default($k = []) { %h{$k}.raku }
is key-default(), 'Any', '... and a plain hash too';

sub array-default(@a = [1, 2]) { @a.raku }
is array-default(), '[1, 2]', 'an @ parameter default is not itemized';
