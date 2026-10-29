use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing, with the names
# declared in the calling scope and with the names declared inside the
# evaluated unit. Family: the `.=` (dotty assignment) expansion for each shape
# of target: a scalar, array and hash variable, an element, a chain, a
# grouped or `.self` target, an inline declaration, the topic and a method
# call with arguments or adverbs.

my $s = "abc";
my @a = 3, 1, 2;
my %h = k => "v";

# Declared inside the unit: the setup is part of the snippet.
my @inside =
    'scalar',         'my $x = "abc"; $x .= uc; $x',                                          'ABC',
    'scalar args',    'my $x = "a-b"; $x .= subst("-", "+"); $x',                              'a+b',
    'array',          'my @l = 3, 1, 2; @l .= sort; @l.join(",")',                             '1,2,3',
    'hash',           'my %m = a => 1; %m .= antipairs; %m.keys.join(",")',                    '1',
    'element',        'my @l = "a", "b"; @l[1] .= uc; @l.join(",")',                           'a,B',
    'hash element',   'my %m = k => "v"; %m<k> .= uc; %m<k>',                                  'V',
    'chain',          'my $x = "abc"; $x .= uc .= flip; $x',                                   'CBA',
    'element chain',  'my @l = "ab"; @l[0] .= uc .= flip; @l[0]',                              'BA',
    'grouped',        'my $x = "abc"; ($x) .= uc; $x',                                         'ABC',
    'assign grouped', 'my $x; ($x = "abc").=uc; $x',                                           'ABC',
    'inline decl',    '(my $x = "abc") .= uc; $x',                                             'ABC',
    'topic',          'my $v = "abc"; given $v { .=uc }; $v',                                  'ABC',
    'topic array',    'my @l = 2, 1; given @l { .=sort }; @l.join(",")',                       '1,2',
    'topic var',      'my $r = "abc"; for $r { $_ .= uc }; $r',                                'ABC',
    'colon args',     'my $x = "a,b"; $x .= split: ","; $x.join("|")',                        'a|b',
    'adverb',         'my %m = k => "aXa"; %m<k> .= subst("a", "b"):g; %m<k>',                 'bXb',
    'new',            'my $x = Int; $x .= new; $x',                                            '0',
    'typed',          'my Str $x = "q"; $x .= uc; $x',                                         'Q';

# Declared in the calling scope: the snippet only names them.
my @outside =
    'scalar',  '$s = "abc"; $s .= uc; $s',                                                     'ABC',
    'array',   '@a = 3, 1, 2; @a .= sort; @a.join(",")',                                       '1,2,3',
    'hash el', '%h<k> = "v"; %h<k> .= uc; %h<k>',                                              'V',
    'chain',   '$s = "abc"; $s .= uc .= flip; $s',                                             'CBA';

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
