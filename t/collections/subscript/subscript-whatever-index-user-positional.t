use Test;

# A WhateverCode position (`$o[*-1]`) on a user Positional is computed against
# its `.elems`, also when the class only answers AT-POS and has no `keys`
# method (Trie's `$t[*-1]`). It used to read Nil.

plan 3;

class P does Positional {
    method elems { 3 }
    method AT-POS($i) { "at$i" }
}
is P.new[*-1], 'at2', '*-1 with an elems method';
is P.new[*-3], 'at0', '*-3';

class Q does Positional {
    has atomicint $.elems = 3;
    method AT-POS($i) { "at$i" }
}
is Q.new[*-1], 'at2', 'with an elems accessor';
