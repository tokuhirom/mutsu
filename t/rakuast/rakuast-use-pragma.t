use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST `use` pragmas (issue #8035). Rakudo keeps an argument-less pragma
# directly in the StatementList as RakuAST::Pragma, while ordinary modules use
# a distinct Statement::Use node.

plan 15;

is Q[use strict].AST.gist, q:to/END/.chomp, 'use strict renders as a Pragma';
RakuAST::StatementList.new(
  RakuAST::Pragma.new(
    name => "strict"
  )
)
END

is Q[use fatal].AST.gist, q:to/END/.chomp, 'use fatal renders as a Pragma';
RakuAST::StatementList.new(
  RakuAST::Pragma.new(
    name => "fatal"
  )
)
END

my $pragma = Q[use strict].AST.statements[0];
is $pragma.^name, 'RakuAST::Pragma', 'pragma is a direct statement node';
is $pragma.name, 'strict', 'pragma name is a Str field';
nok $pragma.argument.defined, 'argument is omitted for an argument-less pragma';
nok $pragma.off, 'off is false for use';
ok $pragma ~~ RakuAST::Statement, 'pragma retains the Statement hierarchy';

my @methods = RakuAST::Pragma.^methods(:local)>>.name;
ok 'new' (elem) @methods, 'Pragma exposes a constructor';
ok 'name' (elem) @methods && 'argument' (elem) @methods && 'off' (elem) @methods,
    'Pragma exposes its field accessors';

my $constructed = RakuAST::Pragma.new(name => 'strict');
is $constructed.gist, q:to/END/.chomp, 'Pragma.new constructs the measured shape';
RakuAST::Pragma.new(
  name => "strict"
)
END
is $constructed.name, 'strict', 'constructed pragma exposes its name';
nok $constructed.argument.defined, 'constructed pragma defaults argument to Nil';
nok $constructed.off, 'constructed pragma defaults off to False';

is EVAL(Q[use strict; 40 + 2].AST), 42,
    'a parsed pragma AST lowers through the existing compiler';
dies-ok { EVAL(Q[use fatal; fail "boom"].AST) },
    'a fatal pragma AST preserves existing failure semantics';
