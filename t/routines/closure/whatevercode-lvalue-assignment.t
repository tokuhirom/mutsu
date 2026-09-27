use Test;

plan 2;

# Configuration 0.0.11 stores a WhateverCode through an rw accessor. Its
# parser shape also exposed the same missing wrapping for indexed binds.
class ConfigLike {
    has $.value is rw;
}

my $config = ConfigLike.new;
$config.value = * + 1;
is $config.value.(1), 2, 'rw accessor assignment preserves WhateverCode';

my %data;
%data<value> := * + 1;
is %data<value>.(1), 2, 'indexed bind preserves WhateverCode';
