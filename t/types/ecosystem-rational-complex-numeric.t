use Test;

plan 2;

is (1 / 100000).Numeric.Rat.nude, (1, 100000),
    'Numeric.Rat preserves an exact rational denominator';
is (1 + 1i).Numeric.^name, 'Complex',
    '.Numeric preserves a Complex value instead of dropping its imaginary part';
