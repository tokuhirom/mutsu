use Test;

plan 10;

# zprintf's `%g`/`%G` fixed-notation output strips trailing zeros (and a bare
# trailing decimal point), like standard C `%g`: a zero renders as "0", not
# "0.000000". The `#` (alternate) flag keeps the zeros. Covers the
# roast S32-str/sprintf.t "zprintf with Numeric/Str type objects" subtest.

is zprintf('%g',   0),      '0',    '%g of 0 strips to "0"';
is zprintf('%.2g', 0),      '0',    '%.2g of 0 strips to "0"';
is zprintf('%G',   0),      '0',    '%G of 0 strips to "0"';
is zprintf('%g',   3e0),    '3',    '%g of 3.0 strips to "3"';
is zprintf('%.4g', 3.1e0),  '3.1',  '%.4g of 3.1 strips trailing zeros';
is zprintf('%.2g', 3.1415), '3.14', '%.2g keeps significant fraction digits';

# A type object (undefined) formats as 0 under `quietly`.
is (quietly zprintf('%g',   Int)), '0', '%g of a type object is "0"';
is (quietly zprintf('%.2g', Num)), '0', '%.2g of a type object is "0"';

# The `#` flag keeps the trailing zeros.
is zprintf('%#.2g', 0),     '0.00', '%#.2g keeps trailing zeros';
is zprintf('%#g',   3e0),   '3.000000', '%#g keeps trailing zeros';
