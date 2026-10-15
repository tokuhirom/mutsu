use Test;

plan 3;

isa-ok Q[my &emit = -> $x { $x }; supply { emit 1 }].AST.statements[1].expression,
    RakuAST::StatementPrefix::Supply, 'the lexical declaration still reads as a supply prefix';

is EVAL(Q[my @calls; { my &emit = -> $x { @calls.push($x) }; my $s = supply { emit 42; }; $s.tap({}); @calls.join(',') }].AST),
    '42', 'a lexical emit in the enclosing block shadows supply rewriting';

is EVAL(Q[my @calls; my $s = supply { my &emit = -> $x { @calls.push($x) }; emit 7; }; $s.tap({}); @calls.join(',')].AST),
    '7', 'a lexical emit declared in the supply body shadows supply rewriting';
