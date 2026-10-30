use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing. Family: word
# quotes (qw, qqw, qqww, << >>, « », %h«...») and their interpolating forms.

my @cases =
    'qw',            'qw<a b c>.raku',                                                   '("a", "b", "c")',
    'qw one',        'qw<a>.raku',                                                       '("a",)',
    'q:w',           'q:w/a b/.raku',                                                    '("a", "b")',
    'qqw',           'my $x = "p q"; qqw/a $x b/.raku',                                  '("a", "p", "q", "b")',
    'qq:w:v',        'my $x = 5; qq:w:v/$x 1/.raku',                                     '(IntStr.new(5, "5"), IntStr.new(1, "1"))',
    'french',        '(«a b»).raku',                                                     '("a", "b")',
    'french val',    '(«1 b»).map({.^name}).join(",")',                                  'IntStr,Str',
    'french interp', 'my $x = "p"; («a$x b»).raku',                                      '("a", "p", "b")',
    'french quoted', 'my $x = "p"; («"a b" $x»).raku',                                   '("a b", "p")',
    'double angle',  'my $x = "q"; (<<a b$x>>).raku',                                    '("a", "b", "q")',
    'qqww',          'my $x = "z"; qqww/a "b $x" c/.raku',                               '("a", "b z", "c")',
    'subscript',     'my %h = a => 1, b => 2; my $k = "a"; %h«$k»',                      '1',
    'subscript 2',   'my %h = a => 1, b => 2; my $k = "b"; my @v = %h«a $k»; @v.join(",")', '1,2',
    'user list',     'sub list(|) { "mine" }; qw<a b c>.raku',                           '("a", "b", "c")';

sub as-file(Str $code) {
    my $proc = run $*EXECUTABLE, '-e', "say do \{ $code }", :out, :err;
    $proc.out.slurp(:close).trim;
}

for @cases -> $name, $code, $expected {
    is as-file($code), $expected, "$name: as a file";
    is EVAL($code).Str, $expected, "$name: as an EVAL string";
    is EVAL($code.AST).Str, $expected, "$name: as EVAL of the string's .AST";
}

{
    my $q = Q[«a $b "c d"»].AST.statements[0].expression;
    isa-ok $q, RakuAST::QuotedString, 'an interpolating « » is a QuotedString';
    is $q.processors.join(","), "quotewords,val", 'with the quotewords and val processors';
    my $w = Q[qqw/a $b/].AST.statements[0].expression;
    is $w.processors.join(","), "words", 'qqw has the words processor';
}

done-testing;
