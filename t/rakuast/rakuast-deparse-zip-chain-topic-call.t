use Test;
use experimental :rakuast;

plan 14;

# A chain of three or more `Z` operands keeps its list infix (the parser runs
# it as one `zip(...)` call; #14845, measured on rakudo 2026.11).
is Q[((1,2) Z (3,4) Z (5,6)).List].AST.DEPARSE.trim, '((1, 2) Z (3, 4) Z (5, 6)).List', 'Z chain in parentheses';
is Q[say (1 Z 2 Z 3)].AST.DEPARSE.trim, 'say (1 Z 2 Z 3)', 'Z chain as an argument';
is Q[1, 2 Z(&) 3, 4 Z(&) 5].AST.DEPARSE.trim, '1, 2 Z(&) 3, 4 Z(&) 5', 'Zop chain over comma columns';
is-deeply ((1, 2) Z (3, 4) Z (5, 6)).List, ((1, 3, 5), (2, 4, 6)), 'the chain still zips';
is-deeply EVAL(Q[((1, 2) Z+ (3, 4) Z+ (5, 6)).List].AST), (9, 12), 'EVAL of a Zop chain';

# `.(args)` is a call of the topic, `Term::TopicCall(Call::Term)`.
is Q[my @a; @a.map({ .() })].AST.DEPARSE.lines[1], '@a.map({ .() })', '.()';
is Q[my @a; @a.map({ .(1, 2) })].AST.DEPARSE.lines[1], '@a.map({ .(1, 2) })', '.(1, 2)';
is Q[.(1)].AST.statements[0].expression.^name, 'RakuAST::Term::TopicCall', 'as a Term::TopicCall';
is-deeply EVAL(Q[my @g = -> $x { $x * 2 }; @g.map({ .(4) }).List].AST), (8,), 'EVAL of .(4)';

# An angle-subscript assignment inside parentheses keeps its angle form.
my %h;
is Q[(%h<a> = 5).raku].AST.DEPARSE.trim, '%h<a> = 5.raku', 'parenthesised %h<a> = 5';
is Q[say(%h<a> = 5)].AST.DEPARSE.trim, 'say(%h<a> = 5)', 'as an argument';
is (%h<b> = 7).raku, '7', 'and still assigns';

# The compunit's own lexicals are terms.
is Q[GLOBALish.WHO].AST.DEPARSE.trim, 'GLOBALish.WHO', 'GLOBALish';
is Q[GLOBALish].AST.statements[0].expression.^name, 'RakuAST::Term::Name', 'as a Term::Name';
