use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing. Family: the object
# hash `:{ ... }` (Circumfix::HashComposer with :object-hash).

my @cases =
    'keys',       'my %h = :{ 1 => "a", "b" => 2 }; %h.keys.sort.map(*.raku).join(",")', '"1","b"',
    'empty',      'my $h = :{}; $h.^name ~ "/" ~ $h.elems',                              'Hash[Mu,Mu,Any]/0',
    'one entry',  'my $h = :{ 1 => 2 }; $h{1}',                                          '2',
    'object key', 'my $k = Any; my $h = :{ $k => 5 }; $h{$k}',                          '5';

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
    my $hc = Q[:{ 1 => 2 }].AST.statements[0].expression;
    isa-ok $hc, RakuAST::Circumfix::HashComposer, 'an object hash is a HashComposer';
    ok $hc.object-hash, 'with the object-hash flag';
}

done-testing;
