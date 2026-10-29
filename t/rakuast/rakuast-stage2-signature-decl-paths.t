use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing, with the names
# declared in the calling scope and with the names declared inside the
# evaluated unit. Family: signature declarations (`my ($a, @b) = ...`, bare and
# assigned, `my (:$k)`-style named destructuring, native types), alone and as
# the head of a statement modifier or a conditional.

my ($p, $q);
my @src = 10, 20;

# Declared inside the unit: the setup is part of the snippet.
my @inside =
    'assigned',        'my ($a, $b) = 1, 2; $a ~ $b',                                       '12',
    'slurpy tail',     'my ($a, @b) = 1, 2, 3; $a ~ "|" ~ @b.join(",")',                    '1|2,3',
    'bare',            'my ($a, $b); $a = 3; $b = 4; $a + $b',                              '7',
    'swap',            'my ($a, $b) = 1, 2; ($a, $b) = ($b, $a); $a ~ $b',                  '21',
    'typed',           'my (Int $a, Str $b) = 5, "x"; $a ~ $b',                             '5x',
    'native',          'my (int $a, num $b); $a ~ "," ~ $b',                                '0,0',
    'bind',            'my ($a, $b) := (7, 8); $a ~ $b',                                    '78',
    'modifier if',     'my $r = "n"; my ($a, $b) = 1, 2 if 1; $r ~ $a ~ $b',               'n12',
    'modifier for',    'my @l; for 1..2 { my ($x, $y) = $_, $_ * 2; @l.push: $x ~ $y }; @l.join(",")', '12,24',
    'in if head',      'my $r; if (my ($a, $b) = 3, 4) { $r = $a * $b }; $r',               '12',
    'in while',        'my $n = 0; my @r; while ($n < 2) { my ($i, $j) = $n, $n + 1; @r.push: $i ~ $j; $n++ }; @r.join(",")', '01,12';
# TODO: `my ($a, ($b, $c)) = 1, (2, 3)` binds the nested group wrongly (#15452).

# Declared in the calling scope: the snippet only names them.
my @outside =
    'assign to caller', '($p, $q) = 1, 2; $p ~ $q',                                         '12',
    'read caller',      'my ($a, $b) = @src; $a + $b',                                      '30',
    'in if head',       'my $r; if (my ($a, $b) = @src) { $r = $a + $b }; $r',              '30';

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
