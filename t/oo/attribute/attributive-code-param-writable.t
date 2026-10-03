use Test;

plan 4;

# An attributive `:&!code` param is writable, and a `&!code = ...` that ends a
# given/with body stores to the attribute.
class M {
    has &.g = WhateverCode;
    submethod BUILD(:&!g = WhateverCode) {
        without &!g { &!g = -> $n { $n * 2 } }
    }
}
is M.new.g.(4), 8, 'without &!g { &!g = ... } in BUILD';
is M.new(g => -> $n { $n + 1 }).g.(4), 5, 'a supplied code value is kept';

class Q {
    has &.g;
    method s { without &!g { &!g = { 9 } }; self }
    method t { given 1 { &!g = { 10 } }; self }
}
is Q.new.s.g.(), 9, 'without &!g in a method';
is Q.new.t.g.(), 10, '&!g assigned as the last statement of a given body';
