use Test;

plan 10;

# `{ :name(*.method) }` is a hash: the Whatever is the method call's invocant,
# so `.method` is not an implicit-topic call and does not force a block.
is {:s(*.abs)}.WHAT.^name, 'Hash', 'colon pair with a WhateverCode value is a hash';
is {s => *.abs}.WHAT.^name, 'Hash', 'fatarrow with a WhateverCode value is a hash';
is {:err(/Sub/), :status(*.so)}.WHAT.^name, 'Hash',
    'a WhateverCode alongside another pair still composes a hash';

my $h = {:status(*.so), :out('x')};
ok $h<status> ~~ Callable, 'the WhateverCode survives as the value';
ok $h<status>.(1), 'and it is callable';
is $h<out>, 'x', 'the sibling pair is intact';

# Infix multiplication is spelled the same way from the dot, and there the call
# really is on the topic, so these stay blocks.
is {a => 2 * .elems}.WHAT.^name, 'Block', 'infix `*` before a topic call is a block';
my $n = 2;
is {a => $n * .elems}.WHAT.^name, 'Block', 'a variable operand does not change that';

# Unrelated invocants keep working.
is {:s(1.abs)}.WHAT.^name, 'Hash', 'a literal invocant is a hash';
is {a => .key}.WHAT.^name, 'Block', 'a bare implicit-topic call is still a block';
