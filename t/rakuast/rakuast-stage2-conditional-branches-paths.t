use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing, with the names
# declared in the calling scope and with the names declared inside the
# evaluated unit. Family: the branches of `if` / `unless` / `with`: pointy
# parameters bound to the tested value, `else -> $x`, `elsif` / `orwith`
# chains, and the `with` topic and parameter binds.

my $v = 5;
my @arr = 1, 2, 3;
my %h = k => 7;

# Declared inside the unit: the setup is part of the snippet.
my @inside =
    'if pointy',        'my $r; if 7 -> $x { $r = $x * 2 }; $r',                                 '14',
    'if topic pointy',  'my $r; if 7 -> $_ { $r = $_ + 1 }; $r',                                 '8',
    'else pointy',      'my $r; if 0 -> $a { $r = "t" } else -> $e { $r = "e$e" }; $r',          'e0',
    'elsif else',       'my $r; if 0 { $r = 1 } elsif 0 { $r = 2 } else { $r = 3 }; $r',         '3',
    'elsif pointy',     'my $r; if 0 { $r = 1 } elsif 4 -> $y { $r = $y }; $r',                  '4',
    'unless',           'my $r = "n"; unless 0 { $r = "y" }; $r',                                'y',
    'unless pointy',    'my $r = "n"; unless 3 -> $u { $r = "u$u" }; $r',                        'n',
    'unless pointy 0',  'my $r = "n"; unless 0 -> $u { $r = "u$u" }; $r',                        'u0',
    'with topic',       'my $r; with 5 { $r = $_ + 1 }; $r',                                     '6',
    'with param',       'my $r; with 5 -> $w { $r = $w * 3 }; $r',                               '15',
    'with alias',       'my @a = 1, 2; with @a { .push(9) }; @a.join(",")',                      '1,2,9',
    'with element',     'my %h = k => 1; with %h<k> { $_ = 8 }; %h<k>',                          '8',
    'with call',        'sub f { 4 }; my $r; with f() { $r = $_ }; $r',                          '4',
    'without',          'my $r = "n"; without Int { $r = "w" }; $r',                             'w',
    'orwith',           'my $r; with Int { $r = 1 } orwith 6 { $r = $_ } else { $r = 0 }; $r',   '6',
    'with else pointy', 'my $r; with Int { $r = 1 } else -> $e { $r = $e.^name }; $r',           'Int',
    'sub-signature',    'my $r; with (1, 2) -> ($a, $b) { $r = $a + $b }; $r',                   '3';

# Declared in the calling scope: the snippet only names them.
my @outside =
    'if pointy',   'my $r; if $v -> $x { $r = $x + 1 }; $r',                  '6',
    'unless',      'my $r = "n"; unless $v { $r = "y" }; $r',                 'n',
    'with alias',  '@arr = 1, 2, 3; with @arr { .push(4) }; @arr.join(",")',                  '1,2,3,4',
    'with elem',   'with %h<k> { $_ = 9 }; %h<k>',                            '9',
    'with param',  'my $r; with $v -> $w { $r = $w * 2 }; $r',                '10',
    'else pointy', 'my $r; if 0 { $r = 1 } else -> $e { $r = $e }; $r',      '0';

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
