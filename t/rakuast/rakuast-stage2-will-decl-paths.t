use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing. Family: a variable
# declaration with `will` phaser traits, which one lowering expands into the
# declaration followed by a phaser per trait.

my @cases =
    'leave scalar',    'my @l; { my $x will leave { @l.push("L$_") } = 5; }; @l.join(",")',             'L5',
    'leave array',     'my @l; { my @a will leave { @l.push(.elems) } = 1, 2, 3; }; @l.join(",")',      '3',
    'enter',           'my @l; { my $x will enter { @l.push("E") } = 5; @l.push("body"); }; @l.join(",")', 'E,body',
    'enter then leave', 'my @l; { my $x will enter { @l.push("E") } will leave { @l.push("L") } = 1; @l.push("b"); }; @l.join(",")', 'E,b,L',
    'two leaves',      'my @l; { my $x will leave { @l.push("1") } will leave { @l.push("2") } = 1; }; @l.join(",")', '2,1',
    'no initializer',  'my @l; { my $x will leave { @l.push("L") }; }; @l.join(",")',                   'L',
    'chained decl',    'my @l; { my $y will leave { @l.push("Y") } = my $z = 4; }; @l.join(",")',       'Y',
    'topic is value',  'my @l; { my $x will leave { @l.push(.^name) } = "s"; }; @l.join(",")',          'Str';

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
