use Test;

plan 5;

ok (1..5).in-range(3, 'x'), 'value in range returns True with a label';
throws-like { (1..5).in-range(7, 'x') }, X::OutOfRange,
    message => 'x out of range. Is: 7, should be in 1..5',
    'custom label is used in the exception';
throws-like { (1..5).in-range(7) }, X::OutOfRange,
    message => 'Value out of range. Is: 7, should be in 1..5',
    'one argument uses the default label';
is-deeply (1..3).reverse, (3, 2, 1).Seq, 'finite range reverses';
throws-like { (1..Inf).reverse }, X::Cannot::Lazy,
    message => 'Cannot .reverse a lazy list',
    'infinite range cannot reverse';
