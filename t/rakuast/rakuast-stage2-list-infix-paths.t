use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing, with the names
# declared in the calling scope and with the names declared inside the
# evaluated unit. Family: the list-infix normalization of a parenthesized list
# or a listop argument list (`Z` / `X` meta-ops lifted across the comma,
# `minmax`, sequence waypoints, Whatever-curried `X`/`Z`) and the sections of
# a semicolon list (`[a, b; c]`, `(a, b; c)`).

my @caller = 1, 2, 3;

# Declared inside the unit: the setup is part of the snippet.
my @inside =
    'zip in parens',      '(1, 2 Z 3, 4).raku',                                           '((1, 3), (2, 4)).Seq',
    'zip plus',           '(1, 2 Z+ 10, 20).join(",")',                                   '11,22',
    'cross plus',         '(1, 2 X+ 10, 20).join(",")',                                   '11,21,12,22',
    'zip in call args',   '(join "-", (1, 2 Z 3, 4)).raku',                                 '"1-3-2-4"',
    'sequence',           '(1, 2, 4 ... 32).join(",")',                                   '1,2,4,8,16,32',
    'sequence waypoints', '(1, 3 ... 9, 8 ... 4).join(",")',                              '1,3,5,7,9,8,7,6,5,4',
    'curried cross',      '(* X+ *)(1, 2).join(",")',                                     '3',
    'curried zip',        '((* Z+ *) ~~ Code).Str',                                       'True',
    'chained zip',        '(1, 2 Z 3, 4 Z 5, 6).elems',                                   '2',
    'minmax',             '(3, 1, 2 minmax 9, 0).raku',                                   '0..9',
    'feed',               '(1, 2, 3 ==> map(* * 2)).join(",")',                           '2,4,6',
    'array sections',     '[1, 2; 3, 4].raku',                                            '[(1, 2), (3, 4)]',
    'array one section',  '[1, 2, 3;].raku',                                              '[1, 2, 3]',
    'array empty section','[1;; 3].raku',                                                 '[1, Any, 3]',
    'paren sections',     '(1, 2; 3, 4).raku',                                            '((1, 2), (3, 4))',
    'paren trailing',     '(1, 2, 3;).raku',                                              '(1, 2, 3)',
    'paren single',       '(5;).raku',                                                    '5',
    'nested',             '[[1, 2], [3]; 4].raku',                                        '[([1, 2], [3]), 4]';

# Declared in the calling scope: the snippet only names them.
my @outside =
    'zip caller',    '(@caller Z 4, 5, 6).elems',                                         '3',
    'cross caller',  '(@caller X* 2, 3).join(",")',                                       '2,3,4,6,6,9',
    'sections',      '[@caller; 4].raku',                                                 '[[1, 2, 3], 4]',
    'minmax caller', '(@caller minmax 9).raku',                                           '1..9';

sub as-file(Str $code) {
    my $proc = run $*EXECUTABLE, '-e', "say do \{ $code }", :out, :err;
    $proc.out.slurp(:close).trim;
}

for @inside -> $name, $code, $expected {
    is as-file($code), $expected, "$name: as a file";
    is EVAL($code).Str, $expected, "$name: as an EVAL string";
    is EVAL($code.AST).Str, $expected, "$name: as EVAL of the string's .AST";
}

for @outside -> $name, $code, $expected {
    is EVAL($code).Str, $expected, "$name (names in the caller): as an EVAL string";
    is EVAL($code.AST).Str, $expected, "$name (names in the caller): as EVAL of the .AST";
}

done-testing;
