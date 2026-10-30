use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing, with the names
# declared in the calling scope and with the names declared inside the
# evaluated unit. Family: the parameter bind of a pointy block on a control
# statement (`given` / `with` / `while` / `until` / `repeat ... while`):
# aliasing and `is copy` binds, sigilless terms, typed parameters,
# sub-signatures and the explicit `-> $_`.

my @caller = 1, 2, 3;
my $x = 10;

# Declared inside the unit: the setup is part of the snippet.
my @inside =
    'given scalar',     'my $r; given 5 -> $v { $r = $v * 2 }; $r',                                   '10',
    'given alias',      'my @a = 1, 2; given @a -> @p { @p.push(3) }; @a.join(",")',                  '1,2,3',
    'given is copy',    'my @a = 1, 2; given @a -> @p is copy { @p.push(3) }; @a.join(",")',          '1,2',
    'given sigilless',  'my $r; given 7 -> \t { $r = t + 1 }; $r',                                    '8',
    'given typed',      'my $r; given 4 -> Int $n { $r = $n }; $r',                                   '4',
    'given topic',      'my $r; given 3 -> $_ { $r = $_ + 1 }; $r',                                   '4',
    'given sub-sig',    'my $r; given (1, 2) -> ($a, $b) { $r = $a + $b }; $r',                       '3',
    'given code',       'my $r; given { 6 } -> &f { $r = f() }; $r',                                  '6',
    'with copy',        'my $r; with 5 -> $w is copy { $w++; $r = $w }; $r',                          '6',
    'while pointy',     'my @l; my @s = 1, 2, 0, 5; while @s.shift -> $v { @l.push($v) }; @l.join(",")', '1,2',
    'until pointy',     'my @l; my @s = 0, 0, 4; until @s.shift -> $v { @l.push($v) }; @l.elems',     '2',
    'while sigilless',  'my @l; my @s = 3, 4, 0; while @s.shift -> \v { @l.push(v) }; @l.join(",")',  '3,4',
    'repeat while',     'my $n = 0; repeat while $n++ < 3 -> $c { }; $n',                             '4',
    'repeat until',     'my $n = 0; repeat { $n++ } until $n > 2; $n',                                '3';

# Declared in the calling scope: the snippet only names them.
my @outside =
    'given alias',  'given @caller -> @p { @p.push(4) }; @caller.join(",")',                          '1,2,3,4',
    'given scalar', 'my $r; given $x -> $v { $r = $v + 1 }; $r',                                      '11',
    'with param',   'my $r; with $x -> $w { $r = $w * 2 }; $r',                                       '20';

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
    @caller = 1, 2, 3;
    is EVAL($code).Str, $expected, "$name (names in the caller): as an EVAL string";
    @caller = 1, 2, 3;
    is EVAL($code.AST).Str, $expected, "$name (names in the caller): as EVAL of the .AST";
}

done-testing;
