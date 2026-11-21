use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: an argument list that
# starts or ends with an angle-quoted number (`<1+2i>`) follows its range
# inside the brackets; a regex's `<&wc>`, `<wc=&wc>` and `<x=.alpha>` place
# their assertions.

plan 5;
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
is placed(Q[say Int.new(<1+2i>.re)]),
    'Call::Name::WithoutParentheses@0-22 Name@0-3 ArgList@4-22 ApplyPostfix@4-22 Type::Simple@4-7 Name@4-7 Call::Method@8-22 Name@8-11 ArgList@13-21 ApplyPostfix@13-21 ComplexLiteral@13-17 Call::Method@19-21 Name@19-21 ArgList@21-21',
    'an argument list starting inside an angle number';
is placed(Q[say Int.new(<1/2>)]),
    'Call::Name::WithoutParentheses@0-18 Name@0-3 ArgList@4-18 ApplyPostfix@4-18 Type::Simple@4-7 Name@4-7 Call::Method@8-18 Name@8-11 ArgList@13-17 RatLiteral@13-16',
    'an argument list ending with an angle number';
is placed(Q[my &wc = /d/; say "d" ~~ /<&wc>/]),
    'VarDeclaration::Simple@0-12 Initializer::Assign@7-12 QuotedRegex@9-12 Regex::Literal@10-11 Call::Name::WithoutParentheses@14-32 Name@14-17 ArgList@18-32 ApplyInfix@18-32 Infix@22-24 ArgList@18-32 QuotedString@18-21 StrLiteral@19-20 QuotedRegex@25-32 Regex::Assertion::Callable@26-31 Var::Lexical@27-30',
    'a callable assertion';
is placed(Q[my &wc = /d/; say "d" ~~ /<wc=&wc>/]),
    'VarDeclaration::Simple@0-12 Initializer::Assign@7-12 QuotedRegex@9-12 Regex::Literal@10-11 Call::Name::WithoutParentheses@14-35 Name@14-17 ArgList@18-35 ApplyInfix@18-35 Infix@22-24 ArgList@18-35 QuotedString@18-21 StrLiteral@19-20 QuotedRegex@25-35 Regex::Assertion::Alias@26-34 Regex::Assertion::Callable@30-33 Var::Lexical@30-33',
    'an aliased callable assertion';
is placed(Q[say "d" ~~ /<x=.alpha>/]),
    'Call::Name::WithoutParentheses@0-23 Name@0-3 ArgList@4-23 ApplyInfix@4-23 Infix@8-10 ArgList@4-23 QuotedString@4-7 StrLiteral@5-6 QuotedRegex@11-23 Regex::Assertion::Alias@12-22 Regex::Assertion::Named@16-21 Name@16-21',
    'an aliased non-capturing assertion';
