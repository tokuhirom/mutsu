use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing. Family: a class
# attribute declared with a compound initializer (`has $.g //= EXPR`), with or
# without a postfix modifier, which one lowering expands into the declaration
# and the assignment of the operator applied to it.

# Rakudo runs the assignment as the class body runs (`has $!g //= 5` there dies
# with "Cannot modify an immutable Nil value"), so the attribute declarations
# sit in a method, which composes the same attributes without running them.
my @cases =
    'public defined-or',  'class PATH { method m { has $.g //= 5; 1 } }; PATH.^attributes.map(*.name).join(",")',                     '$!g',
    'public or rw',       'class PATH { method m { has $.g is rw ||= 6; 1 } }; PATH.^attributes.map(*.name).join(",")',               '$!g',
    'private plus',       'class PATH { has $.k = 3; method m { has Int $!h += 1; 1 } }; PATH.^attributes.map(*.name).join(",")',     '$!k,$!h',
    'typed',              'class PATH { method m { has Int $.h is rw //= 1; 1 } }; PATH.^attributes.map(*.type.^name).join(",")',     'Int',
    'private concat',     'class PATH { method m { has Str $!h ~= "ab"; 1 } }; PATH.^attributes.map(*.type.^name).join(",")',         'Str',
    'modifier if',        'class PATH { method m { has $!h ||= 5 if 1; 1 } }; PATH.^attributes.map(*.name).join(",")',                '$!h',
    'modifier unless',    'class PATH { method m { has $.g is rw //= 5 unless 0; 1 } }; PATH.^attributes.map(*.name).join(",")',      '$!g',
    'modifier when',      'class PATH { method m { has $!h ~= "x" when 1; 1 } }; PATH.^attributes.map(*.name).join(",")',             '$!h',
    'two attributes',     'class PATH { method m { has $!a //= 1; has $!b //= 2; 1 } }; PATH.^attributes.map(*.name).join(",")',      '$!a,$!b',
    'with a default',     'class PATH { has $.k = 3; method m { has $!h //= 1; 1 } }; PATH.new.k',                                    '3';

sub as-file(Str $code) {
    my $proc = run $*EXECUTABLE, '-e', "say do \{ $code }", :out, :err;
    $proc.out.slurp(:close).trim;
}

# A class is installed in its unit's package, so each path names its own.
sub named(Str $code, Str $path) { $code.subst(:g, 'PATH', $path) }

my $n = 0;
for @cases -> $name, $code, $expected {
    $n++;
    is as-file(named($code, "F$n")), $expected, "$name: as a file";
    is EVAL(named($code, "S$n")).Str, $expected, "$name: as an EVAL string";
    is EVAL(named($code, "A$n").AST).Str, $expected, "$name: as EVAL of the string's .AST";
}

# The read direction: the declaration compound node RakuAST makes of it.
for ('$.g', '$!h') -> $var {
    my $ast = "class C{$n++} \{ has $var //= 5 }".AST;
    like $ast.raku, /'MetaInfix::Assign'/, "$var: ApplyInfix with a MetaInfix::Assign";
    like $ast.raku, /'scope       => "has"'/, "$var: over a has declaration";
}

done-testing;
