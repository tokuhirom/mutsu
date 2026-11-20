use Test;

# Str's search and slice methods answer their plain forms without the
# interpreter (Handler::InterpPureFirst) and every other form through it.
# Each call sits in a loop so the VM's call-site lane answers it after the
# first iteration, and each form must give the same answer there.

plan 24;

my $s = 'hello world';
sub repeat(&code) { (code() for ^3).unique.elems == 1 ?? code() !! 'differs' }

is repeat({ $s.substr(6) }), 'world', 'substr(from)';
is repeat({ $s.substr(0, 5) }), 'hello', 'substr(from, chars)';
is repeat({ $s.substr(1..3) }), 'ell', 'substr(Range) takes the interpreter';
is repeat({ $s.substr(*-5) }), 'world', 'substr(WhateverCode)';
ok repeat({ $s.substr(20).WHAT.^name }) eq 'Failure', 'out-of-range substr is a Failure';
is repeat({ $s.substr(2, *-2) }), 'llo wor', 'substr(from, WhateverCode)';

is repeat({ $s.index('o') }), 4, 'index(needle)';
is repeat({ $s.index('o', 5) }), 7, 'index(needle, pos)';
nok repeat({ $s.index('z').defined }), 'index miss is Nil';
is repeat({ $s.rindex('o') }), 7, 'rindex(needle)';
is repeat({ $s.index('O', :i) }), 4, 'index with :i takes the interpreter';

ok repeat({ $s.contains('lo w') }), 'contains(needle)';
ok repeat({ $s.contains(/w.r/) }), 'contains(Regex)';
ok repeat({ $s.starts-with('hell') }), 'starts-with';
ok repeat({ $s.ends-with('rld') }), 'ends-with';
ok repeat({ $s.starts-with('HELL', :i) }), 'starts-with :i';

ok repeat({ $s.substr-eq('world', 6) }), 'substr-eq(needle, pos)';
nok repeat({ $s.substr-eq('xyz', 6) }), 'substr-eq miss';

is repeat({ $s.comb(3).join('|') }), 'hel|lo |wor|ld', 'comb(Int)';
is repeat({ $s.comb('o').elems }), 2, 'comb(Str)';
is repeat({ $s.comb(/<[aeiou]>/).join }), 'eoo', 'comb(Regex)';
is repeat({ 'hello there'.wordcase }), 'Hello There', 'wordcase';

# A Cool receiver reaches the same rows.
is repeat({ 12345.substr(1, 2) }), '23', 'substr on an Int';
is repeat({ 12345.index('4') }), 3, 'index on an Int';
