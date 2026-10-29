use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing. Family: a `my`
# declaration under a statement modifier (`my $x = 5 if COND`), whose
# declaration takes effect unconditionally while only the initializer is
# gated; one lowering splits it.

my @cases =
    'if true',         'my $x = 5 if 1; $x',                                 '5',
    'if false',        'my $x = 5 if 0; $x.defined ?? "def" !! "undef"',     'undef',
    'unless true',     'my $x = 5 unless 0; $x',                             '5',
    'unless false',    'my $x = 5 unless 1; $x.defined ?? "def" !! "undef"', 'undef',
    'declaration only','my $x if 0; $x.defined ?? "def" !! "undef"',         'undef',
    'array',           'my @a = 1, 2, 3 if 1; @a.join(",")',                 '1,2,3',
    'array false',     'my @a = 1, 2, 3 if 0; @a.elems',                     '0',
    'hash',            'my %h = a => 1 if 1; %h<a>',                         '1',
    'with defined',    'my $x = 7 with 3; $x',                               '7',
    'with undefined',  'my $x = 7 with Int; $x.defined ?? "def" !! "undef"', 'undef',
    'without',         'my $x = 8 without Int; $x',                          '8',
    'binding',         'my ($a, $b) = 1, 2 if 1; "$a$b"',                    '12',
    'binding false',   'my ($a, $b) = 1, 2 if 0; ($a.defined, $b.defined).join(",")', 'False,False',
    'condition once',  'my $n = 0; my $x = 1 if ($n++ < 5); $n',             '1',
    'our',             'our $x = 4 if 1; $x',                                '4',
    'sees earlier',    'my $y = 2; my $x = $y * 3 if $y; $x',                '6';

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
