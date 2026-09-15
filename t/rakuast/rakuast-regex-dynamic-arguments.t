use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: an ordinary method call in a subrule argument keeps
# its RakuAST expression tree and can be lowered back to the regex parser.

plan 37;

my $value = 'a';
my $ast = Q[/<word($value.uc)>/].AST;
my $gist = $ast.gist;
ok $gist.contains('RakuAST::Regex::Assertion::Named::Args'),
    'the dynamic argument remains an argumented subrule';
ok $gist.contains('RakuAST::ApplyPostfix'),
    'the method call remains a postfix application';
ok $gist.contains('RakuAST::Call::Method'),
    'the method call keeps its method-call node';
ok $gist.contains('RakuAST::Name.from-identifier("uc")'),
    'the method name remains accessible in the argument tree';

my $regex = EVAL($ast);
ok $regex ~~ Regex,
    'a RakuAST regex with a method-call argument lowers successfully';

grammar GDynamicMethodArgument {
    token TOP { <word($value.uc)> }
    token word($expected) { $expected }
}
ok GDynamicMethodArgument.parse('A').defined,
    'a method-call argument reaches the subrule matcher';
$value = 'b';
ok GDynamicMethodArgument.parse('B').defined,
    'the dynamic method-call argument observes reassignment at match time';

my $modified_ast = Q[/<word($value.?uc)>/].AST;
my $modified_gist = $modified_ast.gist;
ok $modified_gist.contains('RakuAST::Call::Method'),
    'a modified method-call argument keeps its method-call node';
ok $modified_gist.contains('dispatch => ".?"'),
    'a modified method-call argument keeps its dispatch modifier';

my $modified_regex = EVAL($modified_ast);
ok $modified_regex ~~ Regex,
    'a RakuAST regex with a modified method-call argument lowers successfully';

grammar GDynamicModifiedMethodArgument {
    token TOP { <word($value.?uc)> }
    token word($expected) { $expected }
}
$value = 'a';
ok GDynamicModifiedMethodArgument.parse('A').defined,
    'a modified method-call argument reaches the subrule matcher';
$value = 'b';
ok GDynamicModifiedMethodArgument.parse('B').defined,
    'a modified method-call argument observes reassignment at match time';
ok !GDynamicModifiedMethodArgument.parse('A').defined,
    'a modified method-call argument retains its current lexical value';

my $quoted_ast = Q[/<word($value."uc"())>/].AST;
my $quoted_gist = $quoted_ast.gist;
ok $quoted_gist.contains('RakuAST::Call::QuotedMethod'),
    'a quoted method-call argument keeps its quoted-method node';
ok $quoted_gist.contains('RakuAST::QuotedString.new('),
    'a quoted method-call argument keeps its quoted method name';

my $quoted_regex = EVAL($quoted_ast);
ok $quoted_regex ~~ Regex,
    'a RakuAST regex with a quoted method-call argument lowers successfully';

grammar GDynamicQuotedMethodArgument {
    token TOP { <word($value."uc"())> }
    token word($expected) { $expected }
}
$value = 'a';
ok GDynamicQuotedMethodArgument.parse('A').defined,
    'a quoted method-call argument reaches the subrule matcher';
$value = 'b';
ok GDynamicQuotedMethodArgument.parse('B').defined,
    'a quoted method-call argument observes reassignment at match time';
ok !GDynamicQuotedMethodArgument.parse('A').defined,
    'a quoted method-call argument retains its current lexical value';

my $quoted_name = 'uc';
my $dynamic_quoted_ast = Q[/<word($value."$quoted_name"())>/].AST;
my $dynamic_quoted_gist = $dynamic_quoted_ast.gist;
ok $dynamic_quoted_gist.contains('RakuAST::Call::QuotedMethod'),
    'a dynamic quoted method-call argument keeps its quoted-method node';
ok $dynamic_quoted_gist.contains('RakuAST::Var::Lexical.new("\\$quoted_name")'),
    'a dynamic quoted method-call argument keeps its interpolated name';

my $dynamic_quoted_regex = EVAL($dynamic_quoted_ast);
ok $dynamic_quoted_regex ~~ Regex,
    'a RakuAST regex with a dynamic quoted method-call argument lowers successfully';

grammar GDynamicQuotedMethodNameArgument {
    token TOP { <word($value."$quoted_name"())> }
    token word($expected) { $expected }
}
$value = 'a';
ok GDynamicQuotedMethodNameArgument.parse('A').defined,
    'a dynamic quoted method name reaches the subrule matcher';
$quoted_name = 'lc';
ok GDynamicQuotedMethodNameArgument.parse('a').defined,
    'a dynamic quoted method name observes reassignment at match time';
ok !GDynamicQuotedMethodNameArgument.parse('A').defined,
    'a dynamic quoted method name retains its current lexical value';

my @values = <a b>;
my $index = 0;
my $indexed_ast = Q[/<word(@values[$index])>/].AST;
my $indexed_gist = $indexed_ast.gist;
ok $indexed_gist.contains('RakuAST::Postcircumfix::ArrayIndex'),
    'an indexed argument keeps its array-index postfix';
ok $indexed_gist.contains('RakuAST::Var::Lexical.new("\\@values")'),
    'the indexed argument keeps its array target';
ok $indexed_gist.contains('RakuAST::Var::Lexical.new("\\$index")'),
    'the indexed argument keeps its dynamic index expression';

my $indexed_regex = EVAL($indexed_ast);
ok $indexed_regex ~~ Regex,
    'a RakuAST regex with an indexed argument lowers successfully';

grammar GDynamicIndexedArgument {
    token TOP { <word(@values[$index])> }
    token word($expected) { $expected }
}
ok GDynamicIndexedArgument.parse('a').defined,
    'an indexed argument reaches the subrule matcher';
$index = 1;
ok GDynamicIndexedArgument.parse('b').defined,
    'the indexed argument observes index reassignment at match time';

my $which = True;
my $ternary_ast = Q[/<word($which ?? 'a' !! 'b')>/].AST;
my $ternary_gist = $ternary_ast.gist;
ok $ternary_gist.contains('RakuAST::Ternary.new('),
    'a ternary argument keeps its selector node';
ok $ternary_gist.contains('RakuAST::Var::Lexical.new("\\$which")'),
    'the ternary argument keeps its dynamic condition';
ok $ternary_gist.contains('RakuAST::QuotedString.new('),
    'the ternary argument keeps both quoted branches';

my $ternary_regex = EVAL($ternary_ast);
ok $ternary_regex ~~ Regex,
    'a RakuAST regex with a ternary argument lowers successfully';

grammar GDynamicTernaryArgument {
    token TOP { <word($which ?? 'a' !! 'b')> }
    token word($expected) { $expected }
}
ok GDynamicTernaryArgument.parse('a').defined,
    'a ternary argument reaches the subrule matcher';
$which = False;
ok GDynamicTernaryArgument.parse('b').defined,
    'the ternary argument observes condition reassignment at match time';
