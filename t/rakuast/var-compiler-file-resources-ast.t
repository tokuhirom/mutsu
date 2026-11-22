use Test;
use experimental :rakuast;

plan 11;

# `$?FILE` in a string's `.AST` is a `Var::Compiler::File` holding the unit's
# `EVAL_N` name, and `%?RESOURCES` is a bare `Var::Compiler::Resources`, as
# rakudo parses them (#18689).

my $file = Q|$?FILE|.AST.statements[0].expression;
is $file.^name, 'RakuAST::Var::Compiler::File', '$?FILE node class';
ok $file.file.starts-with('EVAL_'), 'it holds the unit name';
is Q|$?FILE|.AST.DEPARSE.trim, '$?FILE', 'DEPARSE';
ok Q|$?FILE.starts-with("EVAL_")|.AST.EVAL, 'EVAL reads the unit name';

my $line = Q|$?LINE|.AST.statements[0].expression;
is $line.^name, 'RakuAST::Var::Compiler::Line', '$?LINE is unchanged';

my $res = Q|%?RESOURCES|.AST.statements[0].expression;
is $res.^name, 'RakuAST::Var::Compiler::Resources', '%?RESOURCES node class';
is $res.raku, 'RakuAST::Var::Compiler::Resources.new', 'its .raku';
is $res.name, '%?RESOURCES', 'its name';
ok $res ~~ RakuAST::Var::Lexical, 'a lexical variable';
is Q|%?RESOURCES<x>|.AST.DEPARSE.trim, '%?RESOURCES<x>', 'DEPARSE of a subscript';
is RakuAST::Var::Compiler::Resources.new.DEPARSE, '%?RESOURCES', 'a built node deparses';
