use v6;
use Test;

# A `$^name` placeholder used inside a NESTED WhateverCode (one scoped to a
# sub-expression, e.g. a method-call argument) belongs to the enclosing explicit
# block, and the WhateverCode closes over it. mutsu used to sweep it into the
# inner WhateverCode's synthetic signature and die with
# "Placeholder variable '$^name' cannot override existing signature".
#
# Regression pin for the YAMLish battery load path. These cases use `~`
# currying, so they are independent of the `=>` Whatever-currying fix.

plan 6;

# The core shape: outer block has $^k, inner WhateverCode (from the `*`s) closes
# over it. `$^k` is the block's only placeholder -> arity 1 -> map feeds each item.
{
    my @out = <a b>.map({ |(1, 2).map($^k ~ "-" ~ *) });
    is @out.elems, 4, 'nested WhateverCode with an outer placeholder runs';
    is @out.join(','), 'a-1,a-2,b-1,b-2', 'placeholder is closed over correctly';
}

# Two placeholders on the outer block (arity 2 -> .kv feeds key,value pairs),
# with the inner WhateverCode referencing one of them. This is the YAMLish shape.
{
    my %h = (x => 10, y => 20);
    my @out = %h.kv.map({ |$^value.pairs.map($^key ~ "=" ~ *.value) }).sort;
    is @out.elems, 2, 'two-placeholder outer block with nested WhateverCode runs';
    is @out[0], 'x=10', 'first pair closed over the right placeholder';
    is @out[1], 'y=20', 'second pair closed over the right placeholder';
}

# A placeholder block whose nested WhateverCode is immediately invoked still works.
{
    my @out = (3, 4).map({ ($^n * *)(10) });
    is @out.join(','), '30,40', 'invoked nested WhateverCode closes over placeholder';
}
