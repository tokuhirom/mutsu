use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 11;

sub body($source) { $source.AST.statements[0].expression.body }

my $quoted = body(Q|/<lit 'a'>/|);
isa-ok $quoted, RakuAST::Regex::Assertion::Named::RegexArg,
    'a spaced assertion argument is a RegexArg';
is $quoted.name.raku, 'RakuAST::Name.from-identifier("lit")',
    'the assertion keeps its target name';
isa-ok $quoted.regex-arg, RakuAST::Regex::Quote,
    'a quoted argument retains its regex quote';
is $quoted.regex-arg.quoted.segments[0].value, 'a',
    'the quoted text is retained';
isa-ok body(Q|/<lit a>/|).regex-arg, RakuAST::Regex::Literal,
    'a bare argument remains a regex literal';
isa-ok body(Q|/<lit [a]>/|).regex-arg, RakuAST::Regex::Group,
    'a bracketed argument remains a regex group';
isa-ok body(Q|/<lit \w>/|).regex-arg, RakuAST::Regex::CharClass::Word,
    'a character-class argument retains its class';

isa-ok EVAL(Q|/<lit 'a'>/|.AST), Regex,
    'a parsed spaced argument lowers to a regex';
isa-ok EVAL(Q|/<lit [a]>/|.AST), Regex,
    'a parsed group argument lowers to a regex';
isa-ok EVAL(RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::Named::RegexArg.new(
        name => RakuAST::Name.from-identifier('lit'),
        regex-arg => RakuAST::Regex::Literal.new('a'),
    ),
)), Regex, 'a hand-built named regex argument lowers';
is EVAL(Q[grammar GSpacedArg { token TOP { <lit 'a'> }; token lit($s) { $s } }; GSpacedArg.^name].AST),
    'GSpacedArg', 'a grammar declaration with the argument round-trips';
