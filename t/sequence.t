use Test;

plan 44;

# single-term sequences
is ~(1 ... 1), '1', '1 ... 1';
is ~('a' ... 'a'), 'a', "'a' ... 'a'";

# finite arithmetic sequences
is (1 ... 5).join(', '), '1, 2, 3, 4, 5', 'increasing sequence';
is (1 ... -3).join(', '), '1, 0, -1, -2, -3', 'decreasing sequence';
is (1, 3 ... 9).join(', '), '1, 3, 5, 7, 9', 'arithmetic with two seeds';
is (1, 0 ... -3).join(', '), '1, 0, -1, -2, -3', 'decreasing arithmetic with two seeds';
is (1, 3, 5 ... 9).join(', '), '1, 3, 5, 7, 9', 'arithmetic with three seeds';

# geometric sequences
is (1, 3, 9 ... 81).join(', '), '1, 3, 9, 27, 81', 'geometric increasing';
is (81, 27, 9 ... 1).join(', '), '81, 27, 9, 3, 1', 'geometric decreasing';

# closure generator
is (1, { $_ + 2 } ... 9).join(', '), '1, 3, 5, 7, 9', 'closure generator';
is (1, { $_ - 2 } ... -7).join(', '), '1, -1, -3, -5, -7', 'closure generator decreasing';
is (1, 3, 5, { $_ + 2 } ... 13).join(', '), '1, 3, 5, 7, 9, 11, 13', 'seeds then closure';

# alternating closure
is (1, { -$_ } ... 1).join(', '), '1', 'alternating stops at seed match';
is (1, { -$_ } ... 3).[^5].join(', '), '1, -1, 1, -1, 1', 'alternating infinite';

# multi-item RHS
is (1 ... 5, 6, 7).join(', '), '1, 2, 3, 4, 5, 6, 7', 'extra items on RHS';
is (1 ... 5, 'xyzzy', 'plugh').join(', '), '1, 2, 3, 4, 5, xyzzy, plugh', 'extra string items on RHS';

# non-integer endpoint (sequence stops before it)
is (1 ... 5.5).join(', '), '1, 2, 3, 4, 5', 'float endpoint';
is (1, 3 ... 10).[lazy ^10].join(', '), '1, 3, 5, 7, 9', 'arithmetic past endpoint';
is (1, 3, 9 ... 100).[lazy ^10].join(', '), '1, 3, 9, 27, 81', 'geometric past endpoint';

# infinite sequences with slice
is (1 ... *).[^5].join(', '), '1, 2, 3, 4, 5', 'infinite increasing';
is (1, 3 ... *).[^5].join(', '), '1, 3, 5, 7, 9', 'infinite arithmetic';
is (1, 0 ... *).[^5].join(', '), '1, 0, -1, -2, -3', 'infinite decreasing';
is (1, 3, 9 ... *).[^5].join(', '), '1, 3, 9, 27, 81', 'infinite geometric';
is (81, 27, 9 ... *).[^5].join(', '), '81, 27, 9, 3, 1', 'infinite decreasing geometric';
is (1, { $_ + 2 } ... *).[^5].join(', '), '1, 3, 5, 7, 9', 'infinite closure';

# constant sequences
is ('c', 'c' ... *).[^5].join(', '), 'c, c, c, c, c', 'constant string';
is (1, 1 ... *).[^5].join(', '), '1, 1, 1, 1, 1', 'constant number';

# misleading starts
is (1, 1, 1, 2, 3 ... 10).[^10].join(', '), '1, 1, 1, 2, 3, 4, 5, 6, 7, 8', 'misleading then arithmetic';
is (1, 1, 1, 2, 4 ... 16).join(', '), '1, 1, 1, 2, 4, 8, 16', 'misleading then geometric';
is (4, 2, 1, 2, 4 ... 16).join(', '), '4, 2, 1, 2, 4, 8, 16', 'direction change geometric';

# wrong side
is (1, 2 ... 0).[lazy ^3], (), 'wrong side gives empty';

# exclusive sequences (...^)
is (1 ...^ 5).join(', '), '1, 2, 3, 4', 'exclusive sequence';
is (1 ...^ -3).join(', '), '1, 0, -1, -2', 'exclusive decreasing';
is (1, 3, 9 ...^ 81).join(', '), '1, 3, 9, 27', 'exclusive geometric';
is (1, { $_ + 2 } ...^ 9).join(', '), '1, 3, 5, 7', 'exclusive with closure';
is (1 ...^ 1), (), 'exclusive same value';
is (1, 1 ...^ 1), (), 'exclusive constant same value';
is (1, 2 ...^ 0, 'xyzzy', 'plugh').[lazy ^5].join(', '), 'xyzzy, plugh', 'exclusive with extra RHS';
is ~(1 ...^ 0), '1', 'exclusive single past endpoint';
is (4...^5).join(', '), '4', '4...^5 parses as 4 ...^ 5';

# Unicode ellipsis aliases
is (1…5).join(', '), '1, 2, 3, 4, 5', 'unicode … sequence';
is (1, 3…9).join(', '), '1, 3, 5, 7, 9', 'unicode … arithmetic sequence';
is (1…^5).join(', '), '1, 2, 3, 4', 'unicode …^ exclusive sequence';
is (1, 3…^9).join(', '), '1, 3, 5, 7', 'unicode …^ arithmetic exclusive sequence';
