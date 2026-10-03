use Test;

plan 8;

for <pi e tau> -> $n {
    is Q:s|$n|.AST.statements[0].expression.raku,
        qq:to/END/.chomp,
        RakuAST::Term::Name.new(
          RakuAST::Name.from-identifier("$n")
        )
        END
        "$n reads back as Term::Name";
}

is Q|now|.AST.statements[0].expression.raku, 'RakuAST::Term::Named.new("now")',
    'now reads back as Term::Named';
like Q|pi + 1|.AST.statements[0].expression.raku, /'Term::Name.new'/,
    'pi inside an expression is a Term::Name too';

is-approx EVAL(RakuAST::Term::Name.new(RakuAST::Name.from-identifier("pi"))), pi,
    'EVAL of Term::Name pi gives the constant';
is-approx EVAL(RakuAST::Term::Name.new(RakuAST::Name.from-identifier("tau"))), tau,
    'EVAL of Term::Name tau gives the constant';
ok EVAL(RakuAST::Term::Named.new("now")) ~~ Instant, 'EVAL of Term::Named now gives an Instant';
