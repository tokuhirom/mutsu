use Test;

# Compiler variables across RakuAST, measured on rakudo 2026.09: a `$?`
# variable is `Var::Compiler::Lookup("$?NAME")`, `$?LINE` is
# `Var::Compiler::Line(N)`, `$?DISTRIBUTION` a bare
# `Var::Compiler::Distribution`. mutsu rendered `Var::Lexical` (or the folded
# literal). This file passes under both mutsu and raku.

plan 12;

sub expr(Str $src) { $src.AST.statements[0].expression }

is expr(Q[$?PACKAGE]).^name, 'RakuAST::Var::Compiler::Lookup', '$?PACKAGE';
is expr(Q[$?PACKAGE]).name, '$?PACKAGE', '... named';
is expr(Q[$?NL]).^name, 'RakuAST::Var::Compiler::Lookup', '$?NL';
is expr(Q[$?TABSTOP]).^name, 'RakuAST::Var::Compiler::Lookup', '$?TABSTOP';
is expr(Q[$?LINE]).^name, 'RakuAST::Var::Compiler::Line', '$?LINE';
is expr(Q[$?LINE]).line, 1, '... with its line';
is expr(Q[$?DISTRIBUTION]).^name, 'RakuAST::Var::Compiler::Distribution', '$?DISTRIBUTION';
is expr(Q["at $?LINE"]).segments[1].^name, 'RakuAST::Var::Compiler::Line', 'interpolated $?LINE';

is EVAL(Q[$?TABSTOP].AST), 8, '$?TABSTOP round-trips';
is EVAL(Q[$?NL].AST), "\n", '$?NL round-trips';
is RakuAST::Var::Compiler::Line.new(5).EVAL, 5, 'a hand-built Line';
is RakuAST::Var::Compiler::Lookup.new('$?NL').EVAL, "\n", 'a hand-built Lookup';
