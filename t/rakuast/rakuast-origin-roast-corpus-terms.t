use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11, for constructs the roast
# corpus showed unplaced: each case is placed as Rakudo places it.

plan 11;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) unless $class ~~ /Statement/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[my @a; for $@a, Any { }]),
    'VarDeclaration::Simple@0-5 ApplyListInfix@11-19 Infix@14-15 Contextualizer::Item@11-14 Var::Lexical@12-14 Type::Simple@16-19 Name@16-19 Block@20-23 Blockoid@20-23',
    'a variable glued to an item contextualizer';
is placed(Q[module M { our sub foo { } }; M::foo if True;]),
    'Module@0-28 Name@7-8 Block@9-28 Blockoid@9-28 Sub@11-26 Name@19-22 Blockoid@23-26 Call::Name@30-36 Name@30-36 ArgList@36-36 Term::Enum@40-44 Name@40-44',
    'a qualified bare call with a modifier';
is placed(Q[{ my $i **= $i; }]),
    'Block@0-17 Blockoid@0-17 ApplyInfix@2-14 MetaInfix::Assign@8-11 Infix@8-10 ArgList@2-14 VarDeclaration::Simple@2-7 Var::Lexical@12-14',
    'a declaring compound assignment';
is placed(Q[say 1 + .5i]),
    'Call::Name::WithoutParentheses@0-11 Name@0-3 ArgList@4-11 ApplyInfix@4-11 Infix@6-7 ArgList@4-11 IntLiteral@4-5 ApplyPostfix@8-11 RatLiteral@8-10 Postfix@10-11',
    'an imaginary rational starting at its point';
is placed(Q[say !not(not  0)]),
    'Call::Name::WithoutParentheses@0-16 Name@0-3 ArgList@4-16 ApplyPrefix@4-16 Prefix@4-5 Call::Name@5-16 Name@5-8 ArgList@9-15 ApplyPrefix@9-15 Prefix@9-12 IntLiteral@14-15',
    'a call operand of a prefix';
is placed(Q[my $pair = a => 1; $pair.value += 100;]),
    'VarDeclaration::Simple@0-17 Initializer::Assign@9-17 FatArrow@11-17 IntLiteral@16-17 ApplyInfix@19-37 MetaInfix::Assign@31-33 Infix@31-32 ArgList@19-37 ApplyPostfix@19-30 Var::Lexical@19-24 Call::Method@25-30 Name@25-30 ArgList@30-30 IntLiteral@34-37',
    'a method call assigned to';
is placed(Q[my @p; push @p, $[ 21 ... 25 ];]),
    'VarDeclaration::Simple@0-5 Call::Name::WithoutParentheses@7-30 Name@7-11 ArgList@12-30 Var::Lexical@12-14 Contextualizer::Item@16-30 Circumfix::ArrayComposer@17-30 SemiList@19-28 ApplyListInfix@19-28 Infix@22-25 IntLiteral@19-21 IntLiteral@26-28',
    'an itemized array composer';
is placed(Q[my $meth = method () { 1 }; my $o; say $o.$meth]),
    'VarDeclaration::Simple@0-26 Initializer::Assign@9-26 Method@11-26 Signature@19-19 Blockoid@21-26 IntLiteral@23-24 VarDeclaration::Simple@28-33 Call::Name::WithoutParentheses@35-47 Name@35-38 ArgList@39-47 ApplyPostfix@39-47 Var::Lexical@39-41 Call::TermAsMethod@42-47 Var::Lexical@42-47 ArgList@47-47',
    'a term called as a method, and an empty method signature';
is placed(Q[my $i; say defined(sink $i = 5)]),
    'VarDeclaration::Simple@0-5 Call::Name::WithoutParentheses@7-31 Name@7-10 ArgList@11-31 Call::Name@11-31 Name@11-18 ArgList@19-30 ApplyInfix@24-30 Assignment@27-28 ArgList@24-30 Var::Lexical@24-26 IntLiteral@29-30',
    'an assignment under a statement prefix';
is placed(Q[my %h; say %h{*}»[1]]),
    'VarDeclaration::Simple@0-5 Call::Name::WithoutParentheses@7-20 Name@7-10 ArgList@11-20 ApplyPostfix@11-20 ApplyPostfix@11-16 Var::Lexical@11-13 Postcircumfix::HashIndex@13-16 SemiList@14-15 Term::Whatever@14-15 MetaPostfix::Hyper@16-20 Postcircumfix::ArrayIndex@17-20 SemiList@18-19 IntLiteral@18-19',
    'a hyper subscript';
is placed(Q[grammar G { rule R { <?> } }]),
    'Grammar@0-28 Name@8-9 Block@10-28 Blockoid@10-28 RuleDeclaration@12-26 Name@17-18 Regex::WithWhitespace@21-25 Regex::Assertion::Pass@21-24',
    'a regex pass assertion';
