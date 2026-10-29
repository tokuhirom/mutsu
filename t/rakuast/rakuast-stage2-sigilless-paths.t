use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing. Family: a
# sigilless declaration (`my \x = ...`, `:=`), whose one lowering decides
# between a readonly term and an alias of the container on the right.

my @cases =
    'readonly literal', 'my \x = 5; x',                                  '5',
    'bind literal',     'my \x := 6; x',                                 '6',
    'alias variable',   'my $a = 5; my \x := $a; x = 10; $a',            '10',
    'alias element',    'my @a = 1, 2; my \e := @a[1]; e = 9; @a.join(",")', '1,9',
    'chain',            'my $a = 1; my \y := $a; my \x := y; x = 3; $a', '3',
    'inner decl',       'my \foo = my $x = 3; foo = 5; $x',              '5',
    'typed',            'my Int \t = 4; t',                              '4',
    'readonly dies',    'my \x = 1; my $r = "alive"; try { x = 2; CATCH { default { $r = "dead" } } }; $r', 'dead';

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
