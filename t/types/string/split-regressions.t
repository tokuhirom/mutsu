use Test;

plan 7;

is 'a1b24f'.split(/\d+/, *).join('|'),
   'a|b|f',
   'regex split treats * limit as unlimited';

is split(/\d+/, 'a1b24f', *).join('|'),
   'a|b|f',
   'split() function treats * limit as unlimited';

is 'theXbigXbang'.split(/X/, -1).elems,
   0,
   'negative split limit returns empty list';

is 'theXbigXbang'.split(/X/, 0).elems,
   0,
   'zero split limit returns empty list';

throws-like { split 'o', 'o', NaN },
  X::TypeCheck::Argument,
  'NaN split limit throws a type check error';

ok 'ab34d5z' ~~ /<.before \d>/,
   '<.before ...> lookaround matches as zero-width assertion';

is 'ab34d5z'.split(/<.before \d>/).join('|'),
   'ab|3|4d|5z',
   'split handles <.before ...> zero-width assertions';
