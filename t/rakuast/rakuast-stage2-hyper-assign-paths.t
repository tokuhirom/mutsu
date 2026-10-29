use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing, with the names
# declared in the calling scope and with the names declared inside the
# evaluated unit. Family: the hyper assignment `TARGET »=» VALUE` and its dwim
# spellings: a plain target (an ordinary hyper op) and a literal list of
# lvalues (an element-wise destructuring, nested sublists included).

my ($p, $q);
my @caller = 1, 2, 3;

# Declared inside the unit: the setup is part of the snippet.
my @inside =
    'array broadcast',   'my @a = 1, 2, 3; @a »=» 7; @a.join(",")',                           '7,7,7',
    'array list',        'my @a = 1, 2, 3; @a »=» (4, 5, 6); @a.join(",")',                    '4,5,6',
    'dwim both',         'my @a = 1, 2, 3; @a «=» (9, 8); @a.join(",")',                       '9,8,9',
    'list targets',      'my ($x, $y); ($x, $y) »=» (5, 6); $x ~ $y',                          '56',
    'list broadcast',    'my ($x, $y); ($x, $y) »=» 5; $x ~ $y',                               '55',
    'nested targets',    'my ($a, $b, $c, $d); (($a, ($b, $c)), $d) »=« ((4, (5, 6)), 7); $a ~ $b ~ $c ~ $d', '4567',
    'array in list',     'my @a = 1, 2; my $s; (@a, $s) »=» 5; @a.join(",") ~ "|" ~ $s',      '5,5|5',
    'declared targets',  '(my $a, my $b) »=» (1, 2); $a ~ $b',                                  '12',
    'value is list',     'my ($x, $y); my $r = (($x, $y) »=» (5, 6)); $r.join(",")',          '5,6',
    'hash target',       'my %h = a => 1, b => 2; %h »=» 0; %h.values.sum',                    '0';

# TODO: `@a «=« (9, 8)` loses the unreached left element (#15527).

# Declared in the calling scope: the snippet only names them.
my @outside =
    'list targets', '($p, $q) »=» (7, 8); $p ~ $q',                                           '78',
    'array',        '@caller = 1, 2, 3; @caller »=» 0; @caller.join(",")',                    '0,0,0';

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
