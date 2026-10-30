use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing. Family: a scalar
# declaration followed by sunk list items (`my $x = 1, 2`), one comma
# application whose lowering expands into the declaration and the items.

my @cases =
    'one item',       'my $x = 5, 6; $x',                               '5',
    'value is list',  'sub f { my $d = 6, 3 }; f().join(",")',           '6,3',
    'trailing comma', 'my $x = 7,; $x',                                 '7',
    'two items',      'my $x = 1, 2, 3; $x',                            '1',
    'typed',          'my Int $x = 4, 5; $x',                           '4',
    'decl operands',  'my $b = 2, my $c = 3; $b + $c',                  '5',
    'item runs',      'my @l; my $x = 1, @l.push(9); @l.join(",")',     '9',
    'array decl',     'my @a = 1, 2; @a.elems',                         '2';

sub as-file(Str $code) {
    my $proc = run $*EXECUTABLE, '-e', "say do \{ $code }", :out, :err;
    $proc.out.slurp(:close).trim;
}

for @cases -> $name, $code, $expected {
    is as-file($code), $expected, "$name: as a file";
    is EVAL($code).Str, $expected, "$name: as an EVAL string";
    is EVAL($code.AST).Str, $expected, "$name: as EVAL of the string's .AST";
}

done-testing;
