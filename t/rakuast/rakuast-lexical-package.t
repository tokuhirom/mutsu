use Test;

# A lexical (`my`) class or grammar across the RakuAST boundary (ADR-10723
# Stage 1). Measured on rakudo 2026.09: it leads with `scope => "my"`; the
# default `our` scope renders no field. This file passes under both mutsu
# and raku. Distinct names because `.AST` registers the symbol.

plan 7;

sub decl(Str $src) { $src.AST.statements[0].expression }

like decl(Q[my class L1 { }]).gist, /'Class.new(' \s* 'scope => "my",' \s* 'name'/,
    'a my class leads with scope => "my"';
nok decl(Q[class L2 { }]).gist.contains('scope'), 'an our class renders no scope';
like decl(Q[my grammar LG1 { token TOP { a } }]).gist,
    /'Grammar.new(' \s* 'scope => "my",'/, 'a my grammar too';

# Write direction.
is EVAL(Q[my class M1 { method m { 42 } }; M1.new.m].AST), 42, 'a my class round-trips';
is EVAL(Q[my class M2 is Array { }; M2.new.^mro[1].^name].AST), 'Array',
    'with a parent';
is EVAL(Q[{ my class M3 { method v { 1 } } }; ::("M3") ~~ Failure].AST), True,
    'and stays lexical to its block';
is EVAL(Q[my grammar MG1 { token TOP { \d+ } }; MG1.parse("12").Str].AST), '12',
    'a my grammar round-trips';
