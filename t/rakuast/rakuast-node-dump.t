use Test;

# RakuAST::Node.dump and its parts, byte for byte as rakudo 2026.11 prints them
# (#18333). The sink marker and thunk lines are not covered yet.

plan 9;

is Q[say 1 + 2].AST.dump, "StatementList 𝄞 EVAL_0:1 ⎡say 1 + 2⎤\n  Statement::Expression ▪𝄞 EVAL_0:1 ⎡say 1 + 2⎤\n    Call::Name::WithoutParentheses  ⎡say 1 + 2⎤\n      Name 【say】  ⎡say⎤\n      ArgList  ⎡1 + 2⎤\n        ApplyInfix  ⎡1 + 2⎤\n          Infix 【+】  ⎡+⎤\n          ArgList  ⎡1 + 2⎤\n            IntLiteral  ⎡1⎤\n            IntLiteral  ⎡2⎤\n", 'dump of say 1 + 2';
is Q[if $_ { say "yes" } else { die }].AST.dump, "StatementList 𝄞 EVAL_1:1 ⎡if \$_ \{ say \\\"yes\\\" } else \{ die }⎤\n  Statement::If 𝄞 EVAL_1:1 ⎡if \$_ \{ say \\\"yes\\\" } else \{ die }⎤\n    Var::Lexical 【\$_】  ⎡\$_⎤\n    Block  ⎡\{ say \\\"yes\\\" }⎤\n      Blockoid 𝄞 EVAL_1:1 ⎡\{ say \\\"yes\\\" }⎤\n        StatementList 𝄞 EVAL_1:1 ⎡say \\\"yes\\\"⎤\n          Statement::Expression ▪𝄞 EVAL_1:1 ⎡say \\\"yes\\\"⎤\n            Call::Name::WithoutParentheses  ⎡say \\\"yes\\\"⎤\n              Name 【say】  ⎡say⎤\n              ArgList  ⎡\\\"yes\\\"⎤\n                QuotedString  ⎡\\\"yes\\\"⎤\n                  StrLiteral  ⎡yes⎤\n    Block  ⎡\{ die }⎤\n      Blockoid 𝄞 EVAL_1:1 ⎡\{ die }⎤\n        StatementList 𝄞 EVAL_1:1 ⎡die⎤\n          Statement::Expression ▪𝄞 EVAL_1:1 ⎡die⎤\n            Call::Name::WithoutParentheses  ⎡die⎤\n              Name 【die】  ⎡die⎤\n              ArgList  ⎡⎤\n", 'dump of if $_ { say "yes" } else { die }';
is Q[sub f($a, :$b) { $a && $b }].AST.dump, "StatementList 𝄞 EVAL_2:1 ⎡sub f(\$a, :\$b) \{ \$a \&\& \$b }⎤\n  Statement::Expression ▪𝄞 EVAL_2:1 ⎡sub f(\$a, :\$b) \{ \$a \&\& \$b }⎤\n    Sub 𝄞 EVAL_2:1 ⎡sub f(\$a, :\$b) \{ \$a \&\& \$b }⎤\n      Name 【f】  ⎡f⎤\n      Signature  ⎡\$a, :\$b⎤\n        Parameter  ⎡\$a⎤\n          Type::Setting\n            Name 【Any】\n          ParameterTarget::Var  ⎡\$a⎤\n            VarDeclaration::Simple 【\$a】  ⎡\$a⎤\n        Parameter  ⎡:\$b⎤\n          Type::Setting\n            Name 【Any】\n          ParameterTarget::Var  ⎡\$b⎤\n            VarDeclaration::Simple 【\$b】  ⎡\$b⎤\n      Blockoid 𝄞 EVAL_2:1 ⎡\{ \$a \&\& \$b }⎤\n        StatementList 𝄞 EVAL_2:1 ⎡\$a \&\& \$b⎤\n          Statement::Expression ▪𝄞 EVAL_2:1 ⎡\$a \&\& \$b⎤\n            ApplyInfix  ⎡\$a \&\& \$b⎤\n              Infix 【\&\&】  ⎡\&\&⎤\n              ArgList  ⎡\$a \&\& \$b⎤\n                Var::Lexical 【\$a】  ⎡\$a⎤\n                Var::Lexical 【\$b】  ⎡\$b⎤\n", 'dump of sub f($a, :$b) { $a && $b }';
is Q[say "a\tb", <x y>].AST.dump, "StatementList 𝄞 EVAL_3:1 ⎡say \\\"a\\\\tb\\\", <x y>⎤\n  Statement::Expression ▪𝄞 EVAL_3:1 ⎡say \\\"a\\\\tb\\\", <x y>⎤\n    Call::Name::WithoutParentheses  ⎡say \\\"a\\\\tb\\\", <x y>⎤\n      Name 【say】  ⎡say⎤\n      ArgList  ⎡\\\"a\\\\tb\\\", <x y>⎤\n        QuotedString  ⎡\\\"a\\\\tb\\\"⎤\n          StrLiteral  ⎡a\\\\tb⎤\n        QuotedString  ⎡<x y>⎤\n          postprocessors: words, val\n          StrLiteral  ⎡x y⎤\n", 'dump of say "a\\tb", <x y>';

my $o = Q[say 1;
say 2].AST.statements[1].origin;
ok $o.is-key, 'a statement origin is a key one';
is-deeply $o.source.location-of-pos($o.from), (2, 1, 'EVAL_4'), 'location-of-pos: line, column, file';
nok Q[say 1].AST.statements[0].expression.args.origin.is-key, 'a sub-statement origin is not';
is Q[1 + 2].AST.statements[0].expression.infix.dump-markers, '【+】', 'an infix marks its operator';
is Q[say 1].AST.statements[0].dump-markers, '▪𝄞', 'a block statement with a key origin';
