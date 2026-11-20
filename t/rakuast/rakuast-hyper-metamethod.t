use Test;
use MONKEY-SEE-NO-EVAL;

# `@a».^name` is not distributed (the metamethod names the container), but the
# written `».` is still part of the tree: `ApplyPostfix` whose postfix is a
# `MetaPostfix::Hyper` over the `Call::MetaMethod` (#18273).

plan 7;

my $ast = Q|@a>>.^name|.AST.statements[0].expression;
is $ast.^name, 'RakuAST::ApplyPostfix', 'the call is a postfix application';
is $ast.postfix.^name, 'RakuAST::MetaPostfix::Hyper', 'its postfix is a hyper';
is $ast.postfix.postfix.^name, 'RakuAST::Call::MetaMethod', 'over the metamethod call';
is $ast.postfix.postfix.name, 'name', 'with its name';

my $with-args = Q|@a».^can("x")|.AST.statements[0].expression;
is $with-args.postfix.postfix.args.args.elems, 1, 'arguments stay on the metamethod call';

is Q|my $x; $x.^name|.AST.statements[1].expression.postfix.^name, 'RakuAST::Call::MetaMethod',
    'without a hyper the postfix stays the plain metamethod call';

my @a = Int, Str;
is EVAL(Q|@a>>.^name|.AST), 'Array', 'EVAL of the tree still names the container';
