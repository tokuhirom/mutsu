use Test;

# The `.AST` of regex literals that the RakuAST finalizer used to refuse
# ("literal Regex without a declarator tree", #18713). Each expected gist is
# rakudo 2026.11's, joined onto one line.

plan 10;

sub ast-line($src) { $src.AST.gist.lines.map(*.trim).join(" ") }

is ast-line(Q[/:i(0) b/]),
    "RakuAST::StatementList.new( RakuAST::Statement::Expression.new( expression => RakuAST::QuotedRegex.new( body => RakuAST::Regex::Sequence.new( RakuAST::Regex::InternalModifier::IgnoreCase.new( negated => True ), RakuAST::Regex::Literal.new(\"b\") ) ) ) )",
    "/:i(0) b/";
is ast-line(Q[/:1ignorecase b/]),
    "RakuAST::StatementList.new( RakuAST::Statement::Expression.new( expression => RakuAST::QuotedRegex.new( body => RakuAST::Regex::Sequence.new( RakuAST::Regex::InternalModifier::IgnoreCase.new( modifier => \"ignorecase\" ), RakuAST::Regex::Literal.new(\"b\") ) ) ) )",
    "/:1ignorecase b/";
is ast-line(Q[/:s(1) b/]),
    "RakuAST::StatementList.new( RakuAST::Statement::Expression.new( expression => RakuAST::QuotedRegex.new( body => RakuAST::Regex::Sequence.new( RakuAST::Regex::InternalModifier::Sigspace.new, RakuAST::Regex::Literal.new(\"b\") ) ) ) )",
    "/:s(1) b/";
is ast-line(Q[/:!i(1) b/]),
    "RakuAST::StatementList.new( RakuAST::Statement::Expression.new( expression => RakuAST::QuotedRegex.new( body => RakuAST::Regex::Sequence.new( RakuAST::Regex::InternalModifier::IgnoreCase.new( negated => True ), RakuAST::Regex::WithWhitespace.new( RakuAST::Regex::CapturingGroup.new( RakuAST::Regex::Literal.new(\"1\") ) ), RakuAST::Regex::Literal.new(\"b\") ) ) ) )",
    "/:!i(1) b/";
is ast-line(Q[/xa*:!y/]),
    "RakuAST::StatementList.new( RakuAST::Statement::Expression.new( expression => RakuAST::QuotedRegex.new( body => RakuAST::Regex::Sequence.new( RakuAST::Regex::Literal.new(\"x\"), RakuAST::Regex::QuantifiedAtom.new( atom       => RakuAST::Regex::Literal.new(\"a\"), quantifier => RakuAST::Regex::Quantifier::ZeroOrMore.new( backtrack => RakuAST::Regex::Backtrack::Greedy ) ), RakuAST::Regex::Literal.new(\"y\") ) ) ) )",
    "/xa*:!y/";
is ast-line(Q[/a**:?2..4/]),
    "RakuAST::StatementList.new( RakuAST::Statement::Expression.new( expression => RakuAST::QuotedRegex.new( body => RakuAST::Regex::QuantifiedAtom.new( atom       => RakuAST::Regex::Literal.new(\"a\"), quantifier => RakuAST::Regex::Quantifier::Range.new( min       => 2, max       => 4, backtrack => RakuAST::Regex::Backtrack::Frugal ) ) ) ) )",
    "/a**:?2..4/";
is ast-line(Q[/a |& b/]),
    "RakuAST::StatementList.new( RakuAST::Statement::Expression.new( expression => RakuAST::QuotedRegex.new( body => RakuAST::Regex::Alternation.new( RakuAST::Regex::WithWhitespace.new( RakuAST::Regex::Literal.new(\"a\") ), RakuAST::Regex::Literal.new(\"b\") ) ) ) )",
    "/a |\& b/";
is ast-line(Q[/& a/]),
    "RakuAST::StatementList.new( RakuAST::Statement::Expression.new( expression => RakuAST::QuotedRegex.new( body => RakuAST::Regex::Literal.new(\"a\") ) ) )",
    "/\& a/";
is ast-line(Q[/(.) $٠/]),
    "RakuAST::StatementList.new( RakuAST::Statement::Expression.new( expression => RakuAST::QuotedRegex.new( body => RakuAST::Regex::Sequence.new( RakuAST::Regex::WithWhitespace.new( RakuAST::Regex::CapturingGroup.new( RakuAST::Regex::CharClass::Any.new ) ), RakuAST::Regex::BackReference::Positional.new( 0 ) ) ) ) )",
    "/(.) \$٠/";
is ast-line(Q[/a**۳/]),
    "RakuAST::StatementList.new( RakuAST::Statement::Expression.new( expression => RakuAST::QuotedRegex.new( body => RakuAST::Regex::QuantifiedAtom.new( atom       => RakuAST::Regex::Literal.new(\"a\"), quantifier => RakuAST::Regex::Quantifier::Range.new( min => 3, max => 3 ) ) ) ) )",
    "/a**۳/";
