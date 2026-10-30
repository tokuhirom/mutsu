use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing. Family: the
# whole-unit passes over the parsed tree (sink warnings, scope diagnostics,
# declaration docs, the `unit class` header) see the written declaration nodes,
# never a parser-built block, so a destructuring declaration in sink position
# warns nothing, a bound declaration still writes back, and a versioned unit
# header still names its class.

my @cases =
    'destructure in sink', 'my ($a, $b) = 1, 2; "$a$b"',                                          '12',
    'destructure bind',    'my @x = 1, 2; my ($a, $b) := @x; $a + $b',                            '3',
    'typed destructure',   'my Int ($a, $b) = 3, 4; $a + $b',                                     '7',
    'comma decls',         'my $x = 1, my $y = 2; $x + $y',                                       '3',
    'lvalue decl target',  'my $s = "abc"; (my $o = $s).substr-rw(0, 1) = "X"; $o',               'Xbc',
    'attribute list',      'class PATH { has Int ($.a, $.b) }; PATH.^attributes.map(*.name).join(",")', '$!a,$!b';

sub as-file(Str $code) {
    my $proc = run $*EXECUTABLE, '-e', "say do \{ $code }", :out, :err;
    $proc.out.slurp(:close).trim;
}
sub named(Str $code, Str $path) { $code.subst(:g, 'PATH', $path) }

my $n = 0;
for @cases -> $name, $code, $expected {
    $n++;
    is as-file(named($code, "F$n")), $expected, "$name: as a file";
    is EVAL(named($code, "S$n")).Str, $expected, "$name: as an EVAL string";
    is EVAL(named($code, "A$n").AST).Str, $expected, "$name: as EVAL of the string's .AST";
}

# A versioned class header still carries its version.
{
    my $p = run $*EXECUTABLE, '-e', 'class Spelled:ver<1.2> { method m { 5 } }; say Spelled.^ver.Str ~ "/" ~ Spelled.m', :out, :err;
    is $p.out.slurp(:close).trim, '1.2/5', 'a versioned class header keeps its version';
}

done-testing;
