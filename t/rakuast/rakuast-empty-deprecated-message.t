use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 5;

my $source = Q[class EmptyDeprecated7564 { has $.value is DEPRECATED(''); }];
my $declaration = $source.AST.statements.tail.expression;
my $attribute = $declaration.body.body.statement-list.statements[0].expression;
my $trait = $attribute.traits[0];

isa-ok $trait, RakuAST::Trait::Is, 'DEPRECATED remains a Trait::Is';
is $trait.name.canonicalize, 'DEPRECATED', 'the deprecation trait keeps its name';
isa-ok $trait.argument, RakuAST::Circumfix::Parentheses,
    'an explicitly empty message keeps its parentheses';
is EVAL(Q|class EmptyDeprecated7564Eval { has $.value is DEPRECATED(''); }; EmptyDeprecated7564Eval.^attributes(:local)[0].DEPRECATED|.AST),
    '', 'an explicit empty message stays empty after EVAL';
is EVAL(Q|class BareDeprecated7564Eval { has $.value is DEPRECATED; }; BareDeprecated7564Eval.^attributes(:local)[0].DEPRECATED|.AST),
    'something else', 'the bare trait keeps its default message';
