use Test;
use experimental :rakuast;

plan 7;

my $block = Q[sub foo(Code *$block) {}].AST.statements[0].expression.signature.parameters[0];
is $block.type.name.canonicalize, 'Code', 'a scalar slurpy retains its type';
is $block.target.name, '$block', 'the slurpy retains its target';
isa-ok $block.slurpy, RakuAST::Parameter::Slurpy::Flattened,
    'the slurpy marker is flattened';

my $anon = Q[:(& is raw = &say)].AST.statements[0].expression.signature.parameters[0];
is $anon.target.name, '&', 'an anonymous callable keeps the bare sigil';
is $anon.traits[0].name.canonicalize, 'raw', 'its raw trait is retained';
isa-ok $anon.default, RakuAST::Var::Lexical, 'its default is a lexical reference';
is $anon.default.name, '&say', 'the default refers to the callable';
