use Test;
use experimental :rakuast;

# Origins inside regexes and regex declarations, as rakudo 2026.11 places them
# (#15780): a token / regex / rule signature (empty `()` sits at its `)`, and
# `visit-children` visits a regex declaration's signature even when none is
# written), the code block of `<?{ }>` / `<!{ }>` and `** {$n}`, colon-form
# call arguments `<.name: ARGS>`, `%h<key>` as a call argument, `$<word>` /
# `$0` interpolated into a string, a name ending before `'`, the atom an
# interpolation is aliased to (`$foo = [<two>]`) and a single-quoted regex
# literal with a backslash.

plan 12;

sub walk(Str $src) {
    my @l;
    sub w($n) {
        with $n.origin { @l.push($n.^name.subst('RakuAST::', '') ~ '@' ~ .from ~ '-' ~ .to) }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}

is walk(Q｢my token word($value) { $value }｣),
    'StatementList@0-32 Statement::Expression@0-32 TokenDeclaration@0-32 Name@9-13 Signature@14-20 Parameter@14-20 ParameterTarget::Var@14-20 VarDeclaration::Simple@14-20 Regex::WithWhitespace@24-31 Regex::Interpolation@24-30 Var::Lexical@24-30',
    'origins of case 1';
is walk(Q｢my regex two(Int $n, $m) { \d ** {$n} }｣),
    'StatementList@0-39 Statement::Expression@0-39 RegexDeclaration@0-39 Name@9-12 Signature@13-23 Parameter@13-19 Type::Simple@13-16 Name@13-16 ParameterTarget::Var@17-19 VarDeclaration::Simple@17-19 Type::Simple@13-16 Name@13-16 Parameter@21-23 ParameterTarget::Var@21-23 VarDeclaration::Simple@21-23 Regex::WithWhitespace@27-38 Regex::QuantifiedAtom@27-37 Regex::WithWhitespace@27-29 Regex::CharClass::Digit@27-29 Regex::Quantifier::BlockRange@30-37 Block@33-37 Blockoid@33-37 StatementList@34-36 Statement::Expression@34-36 Var::Lexical@34-36',
    'origins of case 2';
is walk(Q｢my rule r() { a }｣),
    'StatementList@0-17 Statement::Expression@0-17 RuleDeclaration@0-17 Name@8-9 Signature@10-10 Regex::WithWhitespace@14-16 Regex::Literal@14-15',
    'origins of case 3';
is walk(Q｢my token t { a }｣),
    'StatementList@0-16 Statement::Expression@0-16 TokenDeclaration@0-16 Name@9-10 Regex::WithWhitespace@13-15 Regex::Literal@13-14',
    'origins of case 4';
is walk(Q｢/ <word> <?{ so $<word> }> <.record: "seen $<word>"> /｣),
    'StatementList@0-54 Statement::Expression@0-54 QuotedRegex@0-54 Regex::Sequence@2-53 Regex::WithWhitespace@2-9 Regex::Assertion::Named@2-8 Name@3-7 Regex::WithWhitespace@9-27 Regex::Assertion::PredicateBlock@9-26 Block@11-25 Blockoid@11-25 StatementList@13-23 Statement::Expression@13-23 ApplyPrefix@13-23 Prefix@13-15 Var::NamedCapture@16-23 QuotedString@18-22 StrLiteral@18-22 Regex::WithWhitespace@27-53 Regex::Assertion::Named::Args@27-52 Name@29-35 ArgList@37-51 QuotedString@37-51 StrLiteral@38-43 Var::NamedCapture@43-50 QuotedString@45-49 StrLiteral@45-49',
    'origins of case 5';
is walk(Q｢/ (\w+) <!{ $0 }> /｣),
    'StatementList@0-19 Statement::Expression@0-19 QuotedRegex@0-19 Regex::Sequence@2-18 Regex::WithWhitespace@2-8 Regex::CapturingGroup@2-7 Regex::QuantifiedAtom@3-6 Regex::CharClass::Word@3-5 Regex::Quantifier::OneOrMore@5-6 Regex::WithWhitespace@8-18 Regex::Assertion::PredicateBlock@8-17 Block@10-16 Blockoid@10-16 StatementList@12-14 Statement::Expression@12-14 Var::PositionalCapture@12-14',
    'origins of case 6';
is walk(Q｢my %h; / <word(%h<primary>)> /｣),
    'StatementList@0-30 Statement::Expression@0-5 VarDeclaration::Simple@0-5 Statement::Expression@7-30 QuotedRegex@7-30 Regex::WithWhitespace@9-29 Regex::Assertion::Named::Args@9-28 Name@10-14 ArgList@15-26 ApplyPostfix@15-26 Var::Lexical@15-17 Postcircumfix::LiteralHashIndex@17-26 QuotedString@18-25 StrLiteral@18-25',
    'origins of case 7';
is walk(Q｢say "seen $<word> and $0";｣),
    'StatementList@0-26 Statement::Expression@0-25 Call::Name::WithoutParentheses@0-25 Name@0-3 ArgList@4-25 QuotedString@4-25 StrLiteral@5-10 Var::NamedCapture@10-17 QuotedString@12-16 StrLiteral@12-16 StrLiteral@17-22 Var::PositionalCapture@22-24',
    'origins of case 8';
is walk(Q｢my $src; say "of '$src' is";｣),
    'StatementList@0-28 Statement::Expression@0-7 VarDeclaration::Simple@0-7 Statement::Expression@9-27 Call::Name::WithoutParentheses@9-27 Name@9-12 ArgList@13-27 QuotedString@13-27 StrLiteral@14-18 Var::Lexical@18-22 StrLiteral@22-26',
    'origins of case 9';
is walk(Q｢my $foo; / a $foo = [<two>] d /｣),
    'StatementList@0-31 Statement::Expression@0-7 VarDeclaration::Simple@0-7 Statement::Expression@9-31 QuotedRegex@9-31 Regex::Sequence@11-30 Regex::WithWhitespace@11-13 Regex::Literal@11-12 Regex::Interpolation@13-28 Var::Lexical@13-17 Regex::WithWhitespace@28-30 Regex::Literal@28-29',
    'origins of case 10';
is walk(Q｢my $foo; / a $foo='x' d /｣),
    'StatementList@0-25 Statement::Expression@0-7 VarDeclaration::Simple@0-7 Statement::Expression@9-25 QuotedRegex@9-25 Regex::Sequence@11-24 Regex::WithWhitespace@11-13 Regex::Literal@11-12 Regex::Interpolation@13-22 Var::Lexical@13-17 Regex::WithWhitespace@22-24 Regex::Literal@22-23',
    'origins of case 11';
is walk(Q｢/'a("\$b")'/｣),
    'StatementList@0-12 Statement::Expression@0-12 QuotedRegex@0-12 Regex::Quote@1-11 QuotedString@1-11 StrLiteral@2-10',
    'origins of case 12';
