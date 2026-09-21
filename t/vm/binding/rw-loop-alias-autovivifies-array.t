use Test;

# A writable loop parameter aliases the source array's element cell. Calling a
# mutating list method on an undefined element must vivify that cell, rather
# than replacing only the loop variable's environment binding.

plan 1;

my @rows = ['x'], Any, ['x'];
for @rows <-> $row {
    prepend $row, 'p' xx 2;
}

is-deeply @rows, [['p', 'p', 'x'], ['p', 'p'], ['p', 'p', 'x']],
    'prepend through a writable loop alias autovivifies the source element';
