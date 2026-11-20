use Test;
use experimental :rakuast;

# Origins measured on rakudo 2026.11 (#15780): a `Type::Definedness` term
# (`HyperWhatever:D`: its `Type::Simple` over the whole, the `Name` before the
# smiley), the parentheses of `:16($n)`, a subscript adverb with a value
# (`:exists(1)`, `:delete(True)`) and a lookaround assertion
# (`<?before X>`, `<!after X>`: the named assertion and its argument).

plan 7;

sub walk(Str $src) {
    my @l;
    sub w($n) {
        with $n.origin { @l.push($n.^name.subst('RakuAST::', '') ~ '@' ~ .from ~ '-' ~ .to) }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}

is walk("say 1 ~~ HyperWhatever:D;"),
    "StatementList\@0-25 Statement::Expression\@0-24 Call::Name::WithoutParentheses\@0-24 Name\@0-3 ArgList\@4-24 ApplyInfix\@4-24 Infix\@6-8 ArgList\@4-24 IntLiteral\@4-5 Type::Definedness\@9-24 Type::Simple\@9-24 Name\@9-22",
    'origins of case 1';
is walk("my \$n = 'ff'; say :16(\$n);"),
    "StatementList\@0-26 Statement::Expression\@0-12 VarDeclaration::Simple\@0-12 Initializer::Assign\@6-12 QuotedString\@8-12 StrLiteral\@9-11 Statement::Expression\@14-25 Call::Name::WithoutParentheses\@14-25 Name\@14-17 ArgList\@18-25 Term::RadixNumber\@18-25 Circumfix::Parentheses\@21-25 SemiList\@22-24 Statement::Expression\@22-24 Var::Lexical\@22-24",
    'origins of case 2';
is walk("my \%h; my \$key; say \%h\{\$key}:exists(1);"),
    "StatementList\@0-39 Statement::Expression\@0-5 VarDeclaration::Simple\@0-5 Statement::Expression\@7-14 VarDeclaration::Simple\@7-14 Statement::Expression\@16-38 Call::Name::WithoutParentheses\@16-38 Name\@16-19 ArgList\@20-38 ApplyPostfix\@20-38 Var::Lexical\@20-22 Postcircumfix::HashIndex\@22-38 SemiList\@23-27 Statement::Expression\@23-27 Var::Lexical\@23-27 ColonPair::Value\@28-38 Circumfix::Parentheses\@35-38 SemiList\@36-37 Statement::Expression\@36-37 IntLiteral\@36-37",
    'origins of case 3';
is walk("my \%h; say \%h<a>:delete(True):!p;"),
    "StatementList\@0-33 Statement::Expression\@0-5 VarDeclaration::Simple\@0-5 Statement::Expression\@7-32 Call::Name::WithoutParentheses\@7-32 Name\@7-10 ArgList\@11-32 ApplyPostfix\@11-32 Var::Lexical\@11-13 Postcircumfix::LiteralHashIndex\@13-32 QuotedString\@14-15 StrLiteral\@14-15 ColonPair::Value\@16-29 Circumfix::Parentheses\@23-29 SemiList\@24-28 Statement::Expression\@24-28 Term::Enum\@24-28 Name\@24-28 ColonPair::False\@29-32",
    'origins of case 4';
is walk("my \$x = 'a'; say 'a' ~~ / <?before <\$x>> /;"),
    "StatementList\@0-43 Statement::Expression\@0-11 VarDeclaration::Simple\@0-11 Initializer::Assign\@6-11 QuotedString\@8-11 StrLiteral\@9-10 Statement::Expression\@13-42 Call::Name::WithoutParentheses\@13-42 Name\@13-16 ArgList\@17-42 ApplyInfix\@17-42 Infix\@21-23 ArgList\@17-42 QuotedString\@17-20 StrLiteral\@18-19 QuotedRegex\@24-42 Regex::WithWhitespace\@26-41 Regex::Assertion::Lookahead\@26-40 Regex::Assertion::Named::RegexArg\@28-39 Name\@28-34 Regex::Assertion::InterpolatedVar\@35-39 Var::Lexical\@36-38",
    'origins of case 5';
is walk("say 'a' ~~ / <!after \\d> a /;"),
    "StatementList\@0-29 Statement::Expression\@0-28 Call::Name::WithoutParentheses\@0-28 Name\@0-3 ArgList\@4-28 ApplyInfix\@4-28 Infix\@8-10 ArgList\@4-28 QuotedString\@4-7 StrLiteral\@5-6 QuotedRegex\@11-28 Regex::Sequence\@13-27 Regex::WithWhitespace\@13-25 Regex::Assertion::Lookahead\@13-24 Regex::Assertion::Named::RegexArg\@15-23 Name\@15-20 Regex::CharClass::Digit\@21-23 Regex::WithWhitespace\@25-27 Regex::Literal\@25-26",
    'origins of case 6';
is walk("say 'ab' ~~ / <?before 'a'> \\w /;"),
    "StatementList\@0-33 Statement::Expression\@0-32 Call::Name::WithoutParentheses\@0-32 Name\@0-3 ArgList\@4-32 ApplyInfix\@4-32 Infix\@9-11 ArgList\@4-32 QuotedString\@4-8 StrLiteral\@5-7 QuotedRegex\@12-32 Regex::Sequence\@14-31 Regex::WithWhitespace\@14-28 Regex::Assertion::Lookahead\@14-27 Regex::Assertion::Named::RegexArg\@16-26 Name\@16-22 Regex::Quote\@23-26 QuotedString\@23-26 StrLiteral\@24-25 Regex::WithWhitespace\@28-31 Regex::CharClass::Word\@28-30",
    'origins of case 7';
