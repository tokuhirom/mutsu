use v6;
use experimental :rakuast;
use Test;

# `use Module ()` and `use Module Empty` import nothing, also after the RakuAST
# round trip: the parentheses of the import list and the `Empty` term survive.

plan 6;

my $paren = Q{use lib 't/lib'; use EmptyImportQB ();}.AST.statements[1];
is $paren.argument.^name, 'RakuAST::Circumfix::Parentheses', '() is a Circumfix::Parentheses argument';
my $empty = Q{use lib 't/lib'; use EmptyImportQA Empty;}.AST.statements[1];
is $empty.argument.^name, 'RakuAST::Term::Name', 'Empty is a Term::Name argument';

is EVAL(Q{use lib 't/lib'; use EmptyImportQB (); EmptyImportQB::LOADED}.AST), 'qb-loaded',
    'use Module () still loads the module';
is EVAL(Q{use lib 't/lib'; use EmptyImportQB (); defined ::('&MAIN')}.AST), False,
    'use Module () imports nothing';
is EVAL(Q{use lib 't/lib'; use EmptyImportQA Empty; EmptyImportQA::LOADED}.AST), 'qa-loaded',
    'use Module Empty still loads the module';
is EVAL(Q{use lib 't/lib'; use EmptyImportQA Empty; defined ::('&MAIN')}.AST), False,
    'use Module Empty imports nothing';
