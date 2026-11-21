use experimental :rakuast;
use Test;

# `.origin` and visit order (#15780), measured on rakudo 2026.11: a
# declaration with a `where` clause visits the clause, the type, the
# initializer and the type again; an attribute trait is placed beside its
# default (whose implicit `Trait::WillBuild` is not written); an empty
# substitution replacement sits where it was (not) written.

plan 6;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[my Int $x where * > 1 = 3]),
    'StatementList@0-25 Statement::Expression@0-25 VarDeclaration::Simple@0-25 ApplyInfix@16-21 Infix@18-19 ArgList@16-21 IntLiteral@20-21 Type::Simple@3-6 Name@3-6 Initializer::Assign@22-25 IntLiteral@24-25 Type::Simple@3-6 Name@3-6',
    'a typed declaration with a where clause and an initializer';
is placed(Q[my Int:D $x where 1]),
    'StatementList@0-19 Statement::Expression@0-19 VarDeclaration::Simple@0-19 IntLiteral@18-19 Type::Definedness@3-8 Type::Simple@3-8 Name@3-6 Type::Definedness@3-8 Type::Simple@3-8 Name@3-6',
    'a definite type with a where clause';
is placed(Q[my Int $x is default(2) where * > 1 = 3]),
    'StatementList@0-39 Statement::Expression@0-39 VarDeclaration::Simple@0-39 ApplyInfix@30-35 Infix@32-33 ArgList@30-35 IntLiteral@34-35 Type::Simple@3-6 Name@3-6 Initializer::Assign@36-39 IntLiteral@38-39 Type::Simple@3-6 Name@3-6 Trait::Is@10-23 Name@13-20 Circumfix::Parentheses@20-23 SemiList@21-22 Statement::Expression@21-22 IntLiteral@21-22',
    'a trait, a where clause and an initializer';
is placed(Q[class O { has $.x is rw = 1 }]),
    'StatementList@0-29 Statement::Expression@0-29 Class@0-29 Name@6-7 Block@8-29 Blockoid@8-29 StatementList@10-27 Statement::Expression@10-27 VarDeclaration::Simple@10-27 Initializer::Assign@24-27 IntLiteral@26-27 Trait::Is@18-23 Name@21-23 IntLiteral@26-27',
    'an attribute trait beside a default';
is placed(Q[class P { has Int $.x is rw = 1 }]),
    'StatementList@0-33 Statement::Expression@0-33 Class@0-33 Name@6-7 Block@8-33 Blockoid@8-33 StatementList@10-31 Statement::Expression@10-31 VarDeclaration::Simple@10-31 Type::Simple@14-17 Name@14-17 Initializer::Assign@28-31 IntLiteral@30-31 Trait::Is@22-27 Name@25-27 IntLiteral@30-31',
    'a typed attribute trait beside a default';
is placed(Q[my $c = "a1"; $c ~~ s:g/\d+//]),
    'StatementList@0-29 Statement::Expression@0-12 VarDeclaration::Simple@0-12 Initializer::Assign@6-12 QuotedString@8-12 StrLiteral@9-11 Statement::Expression@14-29 ApplyInfix@14-29 Infix@17-19 ArgList@14-29 Var::Lexical@14-16 Substitution@20-29 ColonPair::True@21-23 Regex::QuantifiedAtom@24-27 Regex::CharClass::Digit@24-26 Regex::Quantifier::OneOrMore@26-27 QuotedString@28-28 StrLiteral@28-28',
    'an empty substitution replacement';
