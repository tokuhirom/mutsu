use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing, with the names
# declared in the calling scope and with the names declared inside the
# evaluated unit. Family: the compound assignments (`OP=`, the short-circuit
# forms, `.=`, the reverse / bracketed / set metaoperator forms, and subscript
# targets), which one lowering expands.

my $n = 10;
my $u;
my $s = 'ab';
my @arr = 1, 2, 3;
my %h = a => 1;
my $set = <a b>.Set;
my $obj = 'xyz';

# Declared inside the unit: the setup is part of the snippet.
my @inside =
    'plus',            'my $x = 1; $x += 4; $x',                                       '5',
    'concat',          'my $x = "a"; $x ~= "b"; $x',                                   'ab',
    'repeat',          'my $x = "ab"; $x x= 2; $x',                                    'abab',
    'defined-or',      'my $x; $x //= 7; $x',                                          '7',
    'defined-or keep', 'my $x = 3; $x //= 7; $x',                                      '3',
    'logical or',      'my $x = 0; $x ||= 8; $x',                                      '8',
    'logical and',     'my $x = 2; $x &&= 9; $x',                                      '9',
    'undefined plus',  'my $x; $x += 2; $x',                                           '2',
    'elem plus',       'my @a = 1, 2, 3; @a[1] += 5; @a.join(",")',                    '1,7,3',
    'elem defined-or', 'my %h; %h<k> //= 3; %h<k>',                                    '3',
    'elem autoviv',    'my @a; @a[2] += 1; @a.elems',                                  '3',
    'dotty',           'my $x = "abc"; $x .= uc; $x',                                  'ABC',
    'dotty args',      'my $x = "a,b"; $x .= split(","); $x.join("-")',                'a-b',
    'bracketed',       'my $x = 2; $x [*]= 5; $x',                                     '10',
    'zip',             'my @a = 1, 2; @a Z+= 10, 20; @a.join(",")',                    '11,22',
    'set union',       'my $x = <a>.Set; $x (|)= <b>.Set; $x.keys.sort.join(",")',    'a,b',
    'set unicode',     'my $x = <a b>.Set; $x ∖= <a>.Set; $x.keys.join(",")',         'b',
    'word infix',      'my $x = 17; $x mod= 5; $x',                                    '2',
    'attribute',       'class C1PATH { has $.n is rw = 1 }; my $o = C1PATH.new; $o.n += 4; $o.n', '5';

# Declared in the calling scope: the snippet only names them.
my @outside =
    'plus',            '$n = 10; $n += 5; $n',                                         '15',
    'defined-or',      '$u //= 4; $u',                                                 '4',
    'concat',          '$s = "ab"; $s ~= "c"; $s',                                                'abc',
    'elem plus',       '@arr = 1, 2, 3; @arr[0] += 9; @arr.join(",")',                                 '10,2,3',
    'elem defined-or', '%h<z> //= 5; %h<z>',                                           '5',
    'dotty',           '$obj .= uc; $obj',                                             'XYZ',
    'set',             '$set (|)= <c>.Set; $set.keys.sort.join(",")',                  'a,b,c';

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
