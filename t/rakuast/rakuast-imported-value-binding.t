use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
plan 8;

my $ast = Q[use lib 't/lib'; use ImportedDeclarationConstants; e].AST;
my $term = $ast.statements[*-1].expression;
isa-ok $term, RakuAST::Term::Name, 'an imported constant retains the public term-name node';
is $term.name.canonicalize, 'e', 'the term retains its written name';
nok $term.gist.contains('source-bareword'), 'binding metadata does not change the public constructor';
is EVAL($ast), 42, 'the round trip reads the imported value instead of the setting constant';

is EVAL(Q[use lib 't/lib'; { use ImportedDeclarationConstants; e + 1 }].AST), 43,
    'a block-local import retains its value binding';
ok abs(EVAL(Q[use lib 't/lib'; { use ImportedDeclarationConstants; e }; e].AST)
    - 2.718281828459045) < 1e-12, 'the import does not shadow the setting outside its block';
ok abs(EVAL(RakuAST::Term::Name.new(RakuAST::Name.from-identifier('e')))
    - 2.718281828459045) < 1e-12, 'a hand-built setting term retains its ordinary meaning';
is EVAL(Q[use lib 't/lib'; use ImportedDeclarationConstants;
    (role { method value { 12 } }).new.value].AST), 12,
    'package declaration syntax and imported terms round-trip together';
