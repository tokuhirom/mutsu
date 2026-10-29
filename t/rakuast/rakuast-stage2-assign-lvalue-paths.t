use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing, with the names
# declared in the calling scope and with the names declared inside the
# evaluated unit. Family: the assignment-target writebacks (an rw-accessor
# method call, a callable, a list), the hyper prefix and the atomic variable
# fetch and store.

class Outer { has $.x is rw = 1 }
my $outer = Outer.new;
my ($p, $q);
my atomicint $at = 0;

# Declared inside the unit: the setup is part of the snippet.
my @inside =
    'method lvalue',   'class C1PATH { has $.x is rw }; my $o = C1PATH.new; $o.x = 5; $o.x',          '5',
    'method compound', 'class C2PATH { has $.x is rw = 1 }; my $o = C2PATH.new; $o.x += 4; $o.x',    '5',
    'temp method',     'class C3PATH { has $.x is rw = 1 }; my $o = C3PATH.new; sub f { temp $o.x = 9; $o.x }; f() ~ $o.x', '91',
    'list lvalue',     'my ($a, $b); ($a, $b) = 1, 2; $a ~ $b',                               '12',
    'sigilless',       'my \h = { a => 1 }; h<a> = 2; h<a>',                                   '2',
    'hyper prefix',    'my @a = 1, 2, 3; (-« @a).join(",")',                                   '-1,-2,-3',
    'hyper prefix ++', 'my @a = 1, 2; ++« @a; @a.join(",")',                                   '2,3',
    'atomic store',    'my atomicint $x = 0; $x ⚛= 5; ⚛$x',                                    '5';

# Declared in the calling scope: the snippet only names them.
my @outside =
    'method lvalue',   '$outer.x = 5; $outer.x',                                               '5',
    'method compound', '$outer.x = 1; $outer.x += 4; $outer.x',                                '5',
    'list lvalue',     '($p, $q) = 3, 4; $p ~ $q',                                             '34',
    'atomic store',    '$at ⚛= 7; ⚛$at',                                                       '7';

sub as-file(Str $code) {
    my $proc = run $*EXECUTABLE, '-e', "say do \{ $code }", :out, :err;
    $proc.out.slurp(:close).trim;
}

# A class is installed in its unit's package, so each path names its own.
sub named(Str $code, Str $path) { $code.subst(:g, 'PATH', $path) }

for @inside -> $name, $code, $expected {
    is as-file(named($code, 'F')), $expected, "$name: as a file";
    is EVAL(named($code, 'S')).Str, $expected, "$name: as an EVAL string";
    is EVAL(named($code, 'A').AST).Str, $expected, "$name: as EVAL of the string's .AST";
}

for @outside -> $name, $code, $expected {
    is EVAL($code).Str, $expected, "$name (names in the caller): as an EVAL string";
    is EVAL($code.AST).Str, $expected, "$name (names in the caller): as EVAL of the .AST";
}

done-testing;
