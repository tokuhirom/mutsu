use Test;
use experimental :rakuast;
use nqp;

plan 12;

# Accessors the vendored RakuAST::Deparse reads on nodes the parser builds;
# before, any file using these died inside .DEPARSE (#14845).
my $def = 'my Int:D $x = 1'.AST.find-nodes(RakuAST::Type::Definedness)[0];
is $def.through-pragma, False, 'a written smiley is not through a pragma';
is $def.definite, True, 'definite';
is 'my Int:D $x = 1'.AST.DEPARSE.trim, 'my Int:D $x = 1', 'DEPARSE of a :D variable';
is 'sub f(Str:U $s, Int:D :$n) { }'.AST.DEPARSE.trim, 'sub f (Str:U $s, Int:D :$n) { }', 'smileys in a signature';

my $plain = 'subset Pos9 of Int'.AST;
nok $plain.find-nodes(RakuAST::Type::Subset)[0].where.defined, 'a subset without where has no where';
is $plain.DEPARSE.trim, 'subset Pos9 of Int', 'DEPARSE of a plain subset';
is 'subset Pos8 of Int where * > 0'.AST.DEPARSE.trim, 'subset Pos8 of Int where * > 0', 'DEPARSE with where';

my $native = RakuAST::Native.new(:repr("CPointer"));
is $native.repr, 'CPointer', 'repr given';
nok RakuAST::Native.new.repr.defined, 'repr absent is an undefined Str';

my $nqp = 'use nqp; nqp::elems(nqp::list(1,2))'.AST.find-nodes(RakuAST::Nqp)[0];
isa-ok $nqp.args, RakuAST::ArgList, 'Nqp.args is an ArgList node';
is 'use nqp; my $cur := nqp::currentthread();'.AST.DEPARSE.lines.tail, 'my $cur := nqp::currentthread()', 'DEPARSE of a no-argument nqp op';
is 'use nqp; say nqp::elems(nqp::list(1,2))'.AST.DEPARSE.lines.tail, 'say nqp::elems(nqp::list(1, 2))', 'DEPARSE of a nested nqp op';
