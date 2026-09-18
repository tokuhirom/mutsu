use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: dynamic expressions in subrule arguments keep their
# RakuAST expression trees and can be lowered back to the regex parser.

plan 161;

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

my %arguments = primary => 'a', secondary => 'b';
my $argument_key = 'primary';
my $hash_index_ast = Q[/<word(%arguments{$argument_key})>/].AST;
my $hash_index_gist = $hash_index_ast.gist;
ok $hash_index_gist.contains('RakuAST::Postcircumfix::HashIndex'),
    'an associative argument keeps its hash-index postfix';
ok $hash_index_gist.contains('RakuAST::Var::Lexical.new("\\%arguments")'),
    'the hash-index argument keeps its hash target';
ok $hash_index_gist.contains('RakuAST::Var::Lexical.new("\\$argument_key")'),
    'the hash-index argument keeps its dynamic key expression';

my $hash_index_regex = EVAL($hash_index_ast);
ok $hash_index_regex ~~ Regex,
    'a RakuAST regex with a hash-index argument lowers successfully';

grammar GDynamicHashIndexArgument {
    token TOP { <word(%arguments{$argument_key})> }
    token word($expected) { $expected }
}
ok GDynamicHashIndexArgument.parse('a').defined,
    'a hash-index argument reaches the subrule matcher';
$argument_key = 'secondary';
ok GDynamicHashIndexArgument.parse('b').defined,
    'the hash-index argument observes key reassignment at match time';
ok !GDynamicHashIndexArgument.parse('a').defined,
    'the hash-index argument retains its current lexical key';

my %literal_arguments = primary => 'a', secondary => 'b';
my $literal_hash_index_ast = Q[/<word(%literal_arguments<primary>)>/].AST;
my $literal_hash_index_gist = $literal_hash_index_ast.gist;
ok $literal_hash_index_gist.contains('RakuAST::Postcircumfix::LiteralHashIndex'),
    'an angle associative argument keeps its literal-hash-index postfix';
ok $literal_hash_index_gist.contains('RakuAST::Var::Lexical.new("\\%literal_arguments")'),
    'the literal hash-index argument keeps its hash target';
ok $literal_hash_index_gist.contains('RakuAST::StrLiteral.new("primary")'),
    'the literal hash-index argument keeps its word-quoted key';

my $literal_hash_index_regex = EVAL($literal_hash_index_ast);
ok $literal_hash_index_regex ~~ Regex,
    'a RakuAST regex with a literal hash-index argument lowers successfully';

grammar GDynamicLiteralHashIndexArgument {
    token TOP { <word(%literal_arguments<primary>)> }
    token word($expected) { $expected }
}
ok GDynamicLiteralHashIndexArgument.parse('a').defined,
    'a literal hash-index argument reaches the subrule matcher';
%literal_arguments<primary> = 'b';
ok GDynamicLiteralHashIndexArgument.parse('b').defined,
    'a literal hash-index argument observes value reassignment at match time';
ok !GDynamicLiteralHashIndexArgument.parse('a').defined,
    'a literal hash-index argument retains its literal key and current value';

my $callable_value = 'a';
my &decorate = -> $argument { $argument.uc };
my $callable_ast = Q[/<word(&decorate($callable_value))>/].AST;
my $callable_gist = $callable_ast.gist;
ok $callable_gist.contains('RakuAST::Regex::Assertion::Named::Args'),
    'a lexical callable remains an argumented subrule';
ok $callable_gist.contains('RakuAST::ApplyPostfix'),
    'a lexical callable keeps its postfix application';
ok $callable_gist.contains('RakuAST::Var::Lexical.new("\\&decorate")'),
    'a lexical callable keeps its code-variable target';
ok $callable_gist.contains('RakuAST::Call::Term'),
    'a lexical callable keeps its indirect call node';

my $callable_regex = EVAL($callable_ast);
ok $callable_regex ~~ Regex,
    'a RakuAST regex with a lexical callable argument lowers successfully';

grammar GDynamicCallableArgument {
    token TOP { <word(&decorate($callable_value))> }
    token word($expected) { $expected }
}
ok GDynamicCallableArgument.parse('A').defined,
    'a lexical callable argument reaches the subrule matcher';
&decorate = -> $argument { $argument.lc };
ok GDynamicCallableArgument.parse('a').defined,
    'a lexical callable argument observes callable reassignment at match time';
ok !GDynamicCallableArgument.parse('A').defined,
    'a lexical callable argument retains its current callable value';

my $named_ast = Q[/<word(:expected($value))>/].AST;
my $named_gist = $named_ast.gist;
ok $named_gist.contains('RakuAST::ColonPair::Value'),
    'a named colonpair argument keeps its colonpair node';
ok $named_gist.contains('key   => "expected"'),
    'a named colonpair argument keeps its key';
ok $named_gist.contains('value => RakuAST::Circumfix::Parentheses'),
    'a named colonpair argument keeps its parenthesized value';

my $named_pair = RakuAST::ColonPair::Value.new(
    key => 'expected',
    value => RakuAST::Var::Lexical.new('$value'),
);
my $named_constructed_ast = RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::Named::Args.new(
        name => RakuAST::Name.from-identifier('word'),
        args => RakuAST::ArgList.new($named_pair),
        capturing => True,
    ),
);
ok EVAL($named_constructed_ast) ~~ Regex,
    'a constructed named colonpair regex lowers successfully';

grammar GDynamicNamedColonPairArgument {
    token TOP { <word(:expected($value))> }
    token word(:$expected) { $expected }
}
$value = 'a';
ok GDynamicNamedColonPairArgument.parse('a').defined,
    'a named colonpair argument reaches the named subrule parameter';
$value = 'b';
ok GDynamicNamedColonPairArgument.parse('b').defined,
    'a named colonpair argument observes value reassignment at match time';
ok !GDynamicNamedColonPairArgument.parse('a').defined,
    'a named colonpair argument retains its current value';

grammar GDynamicNamedColonPairAdverb {
    token TOP { <word(:expected($value))> }
    token word(:$expected) { :i $expected }
}
$value = 'a';
ok GDynamicNamedColonPairAdverb.parse('A').defined,
    'a named colonpair argument remains compatible with regex adverbs';

my $variable_ast = Q[my $expected = 'a'; /<word(:$expected)>/].AST;
my $variable_gist = $variable_ast.gist;
ok $variable_gist.contains('RakuAST::ColonPair::Variable'),
    'a scalar variable colonpair keeps its colonpair node';
ok $variable_gist.contains('key   => "expected"'),
    'a scalar variable colonpair keeps its key';
ok $variable_gist.contains('RakuAST::Var::Lexical.new("\\$expected")'),
    'a scalar variable colonpair keeps its variable value';

my $variable_pair = RakuAST::ColonPair::Variable.new(
    key => 'expected',
    value => RakuAST::Var::Lexical.new('$value'),
);
my $variable_constructed_ast = RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::Named::Args.new(
        name => RakuAST::Name.from-identifier('word'),
        args => RakuAST::ArgList.new($variable_pair),
        capturing => True,
    ),
);
ok EVAL($variable_constructed_ast) ~~ Regex,
    'a constructed variable colonpair regex lowers successfully';

grammar GDynamicVariableColonPairArgument {
    token TOP { <word(:$value)> }
    token word(:$value) { $value }
}
$value = 'a';
ok GDynamicVariableColonPairArgument.parse('a').defined,
    'a scalar variable colonpair reaches the named subrule parameter';
$value = 'b';
ok GDynamicVariableColonPairArgument.parse('b').defined,
    'a scalar variable colonpair observes value reassignment at match time';
ok !GDynamicVariableColonPairArgument.parse('a').defined,
    'a scalar variable colonpair retains its current value';

my $array_variable_ast = Q[my @expected = <a>; /<word(:@expected)>/].AST;
my $array_variable_gist = $array_variable_ast.gist;
ok $array_variable_gist.contains('RakuAST::ColonPair::Variable'),
    'an array variable colonpair keeps its colonpair node';
ok $array_variable_gist.contains('RakuAST::Var::Lexical.new("\\@expected")'),
    'an array variable colonpair keeps its array value';

my $hash_variable_ast = Q[my %expected = a => 1; /<word(:%expected)>/].AST;
my $hash_variable_gist = $hash_variable_ast.gist;
ok $hash_variable_gist.contains('RakuAST::ColonPair::Variable'),
    'a hash variable colonpair keeps its colonpair node';
ok $hash_variable_gist.contains('RakuAST::Var::Lexical.new("\\%expected")'),
    'a hash variable colonpair keeps its hash value';

my $code_variable_ast = Q[my &expected = -> { True }; /<word(:&expected)>/].AST;
my $code_variable_gist = $code_variable_ast.gist;
ok $code_variable_gist.contains('RakuAST::ColonPair::Variable'),
    'a code variable colonpair keeps its colonpair node';
ok $code_variable_gist.contains('RakuAST::Var::Lexical.new("\\&expected")'),
    'a code variable colonpair keeps its code value';

my $boolean_ast = Q[my $value = 'a'; /<word(:enabled)>/].AST;
my $boolean_gist = $boolean_ast.gist;
ok $boolean_gist.contains('RakuAST::ColonPair::True.new("enabled")'),
    'a bare boolean colonpair keeps its source-level node';
ok !$boolean_gist.contains('RakuAST::FatArrow.new'),
    'a bare boolean colonpair is not flattened to a fat-arrow node';

my $boolean_pair = RakuAST::ColonPair::True.new('enabled');
my $boolean_constructed_ast = RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::Named::Args.new(
        name => RakuAST::Name.from-identifier('word'),
        args => RakuAST::ArgList.new($boolean_pair),
        capturing => True,
    ),
);
ok EVAL($boolean_constructed_ast) ~~ Regex,
    'a constructed bare boolean colonpair regex lowers successfully';

grammar GDynamicBooleanColonPairArgument {
    token TOP { <word(:enabled)> }
    token word(:$enabled) { $enabled }
}
ok GDynamicBooleanColonPairArgument.parse('True').defined,
    'a bare boolean colonpair reaches the named subrule parameter';
ok !GDynamicBooleanColonPairArgument.parse('False').defined,
    'a bare boolean colonpair carries True to the named subrule parameter';

my $negated_boolean_ast = Q[my $value = 'a'; /<word(:!enabled)>/].AST;
my $negated_boolean_gist = $negated_boolean_ast.gist;
ok $negated_boolean_gist.contains('RakuAST::ColonPair::False.new("enabled")'),
    'a negated boolean colonpair keeps its source-level node';
ok !$negated_boolean_gist.contains('RakuAST::FatArrow.new'),
    'a negated boolean colonpair is not flattened to a fat-arrow node';

my $negated_boolean_pair = RakuAST::ColonPair::False.new('enabled');
my $negated_boolean_constructed_ast = RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::Named::Args.new(
        name => RakuAST::Name.from-identifier('word'),
        args => RakuAST::ArgList.new($negated_boolean_pair),
        capturing => True,
    ),
);
ok EVAL($negated_boolean_constructed_ast) ~~ Regex,
    'a constructed negated boolean colonpair regex lowers successfully';

grammar GDynamicNegatedBooleanColonPairArgument {
    token TOP { <word(:!enabled)> }
    token word(:$enabled) { $enabled }
}
ok GDynamicNegatedBooleanColonPairArgument.parse('False').defined,
    'a negated boolean colonpair reaches the named subrule parameter';
ok !GDynamicNegatedBooleanColonPairArgument.parse('True').defined,
    'a negated boolean colonpair carries False to the named subrule parameter';

my $block_ast = Q[/<word(:expected{ $value })>/].AST;
my $block_gist = $block_ast.gist;
ok $block_gist.contains('RakuAST::ColonPair::Value'),
    'a block-valued colonpair argument keeps its value node';
ok $block_gist.contains('value => RakuAST::Block.new('),
    'a block-valued colonpair keeps its direct block value';
ok !$block_gist.contains('value => RakuAST::Circumfix::Parentheses'),
    'a block-valued colonpair does not gain a parenthesized value';

my $block_statements = RakuAST::StatementList.new;
$block_statements.add-statement(
    RakuAST::Statement::Expression.new(
        expression => RakuAST::StrLiteral.new('a'),
    )
);
my $block_pair = RakuAST::ColonPair::Value.new(
    key => 'expected',
    value => RakuAST::Block.new(
        body => RakuAST::Blockoid.new($block_statements),
    ),
);
my $block_constructed_ast = RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::Named::Args.new(
        name => RakuAST::Name.from-identifier('word'),
        args => RakuAST::ArgList.new($block_pair),
        capturing => True,
    ),
);
ok EVAL($block_constructed_ast) ~~ Regex,
    'a constructed block-valued colonpair regex lowers successfully';

grammar GDynamicBlockColonPairArgument {
    token TOP { <word(:expected{ $value })> }
    token word(:$expected) { <?{ $expected() eq $value }> }
}
$value = 'a';
ok GDynamicBlockColonPairArgument.parse('').defined,
    'a block-valued colonpair reaches the named subrule parameter as a Block';
$value = 'b';
ok GDynamicBlockColonPairArgument.parse('').defined,
    'a block-valued colonpair preserves its outer lexical closure';

my $hash_ast = Q[/<word(:expected{ a => $value, b => 2 })>/].AST;
my $hash_gist = $hash_ast.gist;
ok $hash_gist.contains('RakuAST::ColonPair::Value'),
    'a hash-composer colonpair argument keeps its value node';
ok $hash_gist.contains('value => RakuAST::Block.new('),
    'a hash-composer colonpair keeps its direct block value';
ok $hash_gist.contains('RakuAST::ApplyListInfix.new(')
    && $hash_gist.comb('RakuAST::FatArrow.new(').elems >= 2,
    'a hash-composer block keeps its comma-separated pairs';
ok EVAL($hash_ast) ~~ Regex,
    'a constructed hash-composer colonpair regex lowers successfully';

grammar GDynamicHashComposerColonPairArgument {
    token TOP { <word(:expected{ a => $value, b => 2 })> }
    token word(:$expected) {
        <?{ $expected<a> eq $value && $expected<b> == 2 }>
    }
}
$value = 'a';
ok GDynamicHashComposerColonPairArgument.parse('').defined,
    'a hash-composer colonpair reaches the named subrule parameter as a Hash';
$value = 'b';
ok GDynamicHashComposerColonPairArgument.parse('').defined,
    'a hash-composer colonpair observes reassignment at match time';
ok !GDynamicHashComposerColonPairArgument.parse('a').defined,
    'a hash-composer colonpair does not retain its prior lexical value';

my $placeholder_ast = Q[/<word(:expected{ $^candidate eq $value })>/].AST;
my $placeholder_gist = $placeholder_ast.gist;
ok $placeholder_gist.contains('RakuAST::ColonPair::Value'),
    'a placeholder block colonpair keeps its value node';
ok $placeholder_gist.contains('value => RakuAST::Block.new('),
    'a placeholder block colonpair keeps its direct block value';
ok $placeholder_gist.contains('RakuAST::VarDeclaration::Placeholder::Positional.new'),
    'a scalar placeholder remains a positional placeholder declaration';
ok !$placeholder_gist.contains('RakuAST::PointyBlock'),
    'a placeholder block is not reclassified as a PointyBlock';
ok EVAL($placeholder_ast) ~~ Regex,
    'a constructed placeholder block colonpair regex lowers successfully';

my $placeholder = RakuAST::VarDeclaration::Placeholder::Positional.new(q[$candidate]);
my $placeholder_statements = RakuAST::StatementList.new;
$placeholder_statements.add-statement(
    RakuAST::Statement::Expression.new(expression => $placeholder)
);
my $placeholder_pair = RakuAST::ColonPair::Value.new(
    key => 'expected',
    value => RakuAST::Block.new(
        body => RakuAST::Blockoid.new($placeholder_statements),
    ),
);
my $placeholder_constructed_ast = RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::Named::Args.new(
        name => RakuAST::Name.from-identifier('word'),
        args => RakuAST::ArgList.new($placeholder_pair),
        capturing => True,
    ),
);
ok EVAL($placeholder_constructed_ast) ~~ Regex,
    'a hand-built positional placeholder block lowers through regex parsing';

grammar GDynamicPlaceholderBlockColonPairArgument {
    token TOP { <word(:expected{ $^candidate eq $value })> };
    token word(:$expected) { <?{ $expected('a') }> }
}
$value = 'a';
ok GDynamicPlaceholderBlockColonPairArgument.parse('').defined,
    'a placeholder block reaches the named subrule parameter as a Block';
$value = 'b';
ok !GDynamicPlaceholderBlockColonPairArgument.parse('').defined,
    'a placeholder block observes its captured lexical at match time';

my $slurpy_ast = Q[/<word(:expected{ @_ })>/].AST;
my $slurpy_gist = $slurpy_ast.gist;
ok $slurpy_gist.contains('RakuAST::ColonPair::Value'),
    'an array-slurpy placeholder colonpair keeps its value node';
ok $slurpy_gist.contains('value => RakuAST::Block.new('),
    'an array-slurpy placeholder colonpair keeps its direct block value';
ok $slurpy_gist.contains('RakuAST::VarDeclaration::Placeholder::SlurpyArray.new'),
    'an array-slurpy placeholder keeps its source-level declaration';
ok !$slurpy_gist.contains('RakuAST::PointyBlock'),
    'an array-slurpy placeholder block is not reclassified as a PointyBlock';
ok !$slurpy_gist.contains('RakuAST::FatArrow.new'),
    'an array-slurpy placeholder colonpair is not flattened to a fat arrow';
ok EVAL($slurpy_ast) ~~ Regex,
    'a source array-slurpy placeholder colonpair regex lowers successfully';
ok RakuAST::VarDeclaration::Placeholder::SlurpyArray.new.gist
        eq 'RakuAST::VarDeclaration::Placeholder::SlurpyArray.new',
    'the array-slurpy placeholder constructor renders without fields';

my $slurpy_placeholder = RakuAST::VarDeclaration::Placeholder::SlurpyArray.new;
my $slurpy_statements = RakuAST::StatementList.new;
$slurpy_statements.add-statement(
    RakuAST::Statement::Expression.new(expression => $slurpy_placeholder)
);
my $slurpy_pair = RakuAST::ColonPair::Value.new(
    key => 'expected',
    value => RakuAST::Block.new(
        body => RakuAST::Blockoid.new($slurpy_statements),
    ),
);
my $slurpy_constructed_ast = RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::Named::Args.new(
        name => RakuAST::Name.from-identifier('word'),
        args => RakuAST::ArgList.new($slurpy_pair),
        capturing => True,
    ),
);
ok EVAL($slurpy_constructed_ast) ~~ Regex,
    'a hand-built array-slurpy placeholder regex lowers through the matcher';

grammar GDynamicArraySlurpyPlaceholderColonPairArgument {
    token TOP { <word(:expected{ @_ })> };
    token word(:$expected) {
        <?{
            my @arguments = $expected('a', $value);
            @arguments.elems == 2
                && @arguments[0] eq 'a'
                && @arguments[1] eq 'b'
        }>
    }
}
$value = 'b';
ok GDynamicArraySlurpyPlaceholderColonPairArgument.parse('').defined,
    'an array-slurpy placeholder reaches the named subrule as a list';
$value = 'c';
ok !GDynamicArraySlurpyPlaceholderColonPairArgument.parse('').defined,
    'an array-slurpy placeholder keeps its outer lexical dynamic';

my $hash_slurpy_ast = Q[/<word(:expected{ %_ })>/].AST;
my $hash_slurpy_gist = $hash_slurpy_ast.gist;
ok $hash_slurpy_gist.contains('RakuAST::ColonPair::Value'),
    'a hash-slurpy placeholder colonpair keeps its value node';
ok $hash_slurpy_gist.contains('value => RakuAST::Block.new('),
    'a hash-slurpy placeholder colonpair keeps its direct block value';
ok $hash_slurpy_gist.contains('RakuAST::VarDeclaration::Placeholder::SlurpyHash.new'),
    'a hash-slurpy placeholder keeps its source-level declaration';
ok !$hash_slurpy_gist.contains('RakuAST::PointyBlock'),
    'a hash-slurpy placeholder block is not reclassified as a PointyBlock';
ok !$hash_slurpy_gist.contains('RakuAST::Var::Lexical.new("\\%_")'),
    'a hash-slurpy placeholder is not flattened to a lexical variable';
ok EVAL($hash_slurpy_ast) ~~ Regex,
    'a source hash-slurpy placeholder colonpair regex lowers successfully';
ok RakuAST::VarDeclaration::Placeholder::SlurpyHash.new.gist
        eq 'RakuAST::VarDeclaration::Placeholder::SlurpyHash.new',
    'the hash-slurpy placeholder constructor renders without fields';

my $hash_slurpy_placeholder = RakuAST::VarDeclaration::Placeholder::SlurpyHash.new;
my $hash_slurpy_statements = RakuAST::StatementList.new;
$hash_slurpy_statements.add-statement(
    RakuAST::Statement::Expression.new(expression => $hash_slurpy_placeholder)
);
my $hash_slurpy_pair = RakuAST::ColonPair::Value.new(
    key => 'expected',
    value => RakuAST::Block.new(
        body => RakuAST::Blockoid.new($hash_slurpy_statements),
    ),
);
my $hash_slurpy_constructed_ast = RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::Named::Args.new(
        name => RakuAST::Name.from-identifier('word'),
        args => RakuAST::ArgList.new($hash_slurpy_pair),
        capturing => True,
    ),
);
ok EVAL($hash_slurpy_constructed_ast) ~~ Regex,
    'a hand-built hash-slurpy placeholder regex lowers through the matcher';

grammar GDynamicHashSlurpyPlaceholderColonPairArgument {
    token TOP { <word(:expected{ %_ })> };
    token word(:$expected) {
        <?{
            my %arguments = $expected(:first('a'), :second($value));
            %arguments.elems == 2
                && %arguments<first> eq 'a'
                && %arguments<second> eq 'b'
        }>
    }
}
$value = 'b';
ok GDynamicHashSlurpyPlaceholderColonPairArgument.parse('').defined,
    'a hash-slurpy placeholder reaches the named subrule as a hash';
$value = 'c';
ok !GDynamicHashSlurpyPlaceholderColonPairArgument.parse('').defined,
    'a hash-slurpy placeholder keeps its outer lexical dynamic';

my $explicit_signature_ast =
    Q[/<word(:expected(-> $candidate { $candidate eq $value }))>/].AST;
my $explicit_signature_gist = $explicit_signature_ast.gist;
ok $explicit_signature_gist.contains('RakuAST::ColonPair::Value'),
    'an explicit-signature colonpair keeps its value node';
ok $explicit_signature_gist.contains('value => RakuAST::Circumfix::Parentheses.new('),
    'an explicit-signature colonpair keeps its parenthesized value';
ok $explicit_signature_gist.contains('RakuAST::PointyBlock.new('),
    'an explicit-signature colonpair keeps its pointy block';
ok $explicit_signature_gist.contains('RakuAST::ParameterTarget::Var.new('),
    'an explicit-signature colonpair keeps its parameter target';
ok EVAL($explicit_signature_ast) ~~ Regex,
    'a source explicit-signature colonpair regex lowers successfully';

my $explicit_parameter = RakuAST::Parameter.new(
    target => RakuAST::ParameterTarget::Var.new(name => '$candidate'),
);
my $explicit_statements = RakuAST::StatementList.new;
$explicit_statements.add-statement(
    RakuAST::Statement::Expression.new(
        expression => RakuAST::Var::Lexical.new('$candidate'),
    )
);
my $explicit_pair = RakuAST::ColonPair::Value.new(
    key => 'expected',
    value => RakuAST::Circumfix::Parentheses.new(
        RakuAST::SemiList.new(
            RakuAST::Statement::Expression.new(
                expression => RakuAST::PointyBlock.new(
                    signature => RakuAST::Signature.new(
                        parameters => [$explicit_parameter],
                    ),
                    body => RakuAST::Blockoid.new($explicit_statements),
                ),
            ),
        ),
    ),
);
my $explicit_constructed_ast = RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::Named::Args.new(
        name => RakuAST::Name.from-identifier('word'),
        args => RakuAST::ArgList.new($explicit_pair),
        capturing => True,
    ),
);
ok EVAL($explicit_constructed_ast) ~~ Regex,
    'a hand-built explicit-signature regex lowers through the matcher';

grammar GDynamicExplicitSignatureColonPairArgument {
    token TOP { <word(:expected(-> $candidate { $candidate eq $value }))> }
    token word(:$expected) { <.alpha> <?{ $expected('a') }> }
}
$value = 'a';
ok GDynamicExplicitSignatureColonPairArgument.parse('a').defined,
    'an explicit-signature colonpair reaches the named subrule as a callable';
$value = 'b';
ok !GDynamicExplicitSignatureColonPairArgument.parse('a').defined,
    'an explicit-signature colonpair keeps its outer lexical dynamic';

my $other-value = 'b';
my $multi_signature_ast =
    Q[/<word(:expected(-> $candidate, $other { $candidate eq $value && $other eq $other-value }))>/].AST;
my $multi_signature_gist = $multi_signature_ast.gist;
ok $multi_signature_gist.contains('name => "\\$candidate"'),
    'a multi-parameter signature keeps its first parameter';
ok $multi_signature_gist.contains('name => "\\$other"'),
    'a multi-parameter signature keeps its second parameter';
ok EVAL($multi_signature_ast) ~~ Regex,
    'a source multi-parameter signature regex lowers successfully';

my $multi_parameters = [
    RakuAST::Parameter.new(
        target => RakuAST::ParameterTarget::Var.new(name => '$candidate'),
    ),
    RakuAST::Parameter.new(
        target => RakuAST::ParameterTarget::Var.new(name => '$other'),
    ),
];
my $multi_statements = RakuAST::StatementList.new;
$multi_statements.add-statement(
    RakuAST::Statement::Expression.new(
        expression => RakuAST::Var::Lexical.new('$candidate'),
    )
);
my $multi_pair = RakuAST::ColonPair::Value.new(
    key => 'expected',
    value => RakuAST::Circumfix::Parentheses.new(
        RakuAST::SemiList.new(
            RakuAST::Statement::Expression.new(
                expression => RakuAST::PointyBlock.new(
                    signature => RakuAST::Signature.new(parameters => $multi_parameters),
                    body => RakuAST::Blockoid.new($multi_statements),
                ),
            ),
        ),
    ),
);
my $multi_constructed_ast = RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::Named::Args.new(
        name => RakuAST::Name.from-identifier('word'),
        args => RakuAST::ArgList.new($multi_pair),
        capturing => True,
    ),
);
ok EVAL($multi_constructed_ast) ~~ Regex,
    'a hand-built multi-parameter signature regex lowers through the matcher';

grammar GDynamicMultiParameterSignatureColonPairArgument {
    token TOP { <word(:expected(-> $candidate, $other { $candidate eq $value && $other eq $other-value }))> };
    token word(:$expected) { <.alpha> <?{ $expected('a', 'b') }> }
}
$value = 'a';
ok GDynamicMultiParameterSignatureColonPairArgument.parse('a').defined,
    'a multi-parameter signature reaches the named subrule as a callable';
$value = 'c';
ok !GDynamicMultiParameterSignatureColonPairArgument.parse('a').defined,
    'a multi-parameter signature keeps its outer lexical dynamic';

my $typed_signature_ast =
    Q[/<word(:expected(-> Int $candidate { $candidate == 42 }))>/].AST;
my $typed_signature_gist = $typed_signature_ast.gist;
ok $typed_signature_gist.contains('RakuAST::Type::Simple.new('),
    'a typed signature keeps its simple type node';
ok $typed_signature_gist.contains('name => "\\$candidate"'),
    'a typed signature keeps its parameter target';
ok EVAL($typed_signature_ast) ~~ Regex,
    'a source typed-signature regex lowers successfully';

my $typed_parameter = RakuAST::Parameter.new(
    type => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Int')),
    target => RakuAST::ParameterTarget::Var.new(name => '$candidate'),
);
my $typed_statements = RakuAST::StatementList.new;
$typed_statements.add-statement(
    RakuAST::Statement::Expression.new(
        expression => RakuAST::Var::Lexical.new('$candidate'),
    )
);
my $typed_pointy = RakuAST::PointyBlock.new(
    signature => RakuAST::Signature.new(parameters => [$typed_parameter]),
    body => RakuAST::Blockoid.new($typed_statements),
);
my &typed_callable = EVAL($typed_pointy);
is &typed_callable(42), 42,
    'a constructed typed pointy block keeps its callable parameter';
throws-like { &typed_callable('not an Int') }, Exception,
    'a constructed typed pointy block enforces its parameter type';

my $typed_pair = RakuAST::ColonPair::Value.new(
    key => 'expected',
    value => RakuAST::Circumfix::Parentheses.new(
        RakuAST::SemiList.new(
            RakuAST::Statement::Expression.new(expression => $typed_pointy),
        ),
    ),
);
my $typed_constructed_ast = RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::Named::Args.new(
        name => RakuAST::Name.from-identifier('word'),
        args => RakuAST::ArgList.new($typed_pair),
        capturing => True,
    ),
);
ok EVAL($typed_constructed_ast) ~~ Regex,
    'a hand-built typed-signature regex lowers through the matcher';

my $typed_value = 42;
grammar GDynamicTypedSignatureColonPairArgument {
    token TOP { <word(:expected(-> Int $candidate { $candidate == $typed_value }))> };
    token word(:$expected) { <.alpha> <?{ $expected(42) }> }
}
ok GDynamicTypedSignatureColonPairArgument.parse('a').defined,
    'a typed signature reaches the named subrule as a callable';
$typed_value = 41;
ok !GDynamicTypedSignatureColonPairArgument.parse('a').defined,
    'a typed signature keeps its outer lexical dynamic';

my $default_signature_ast =
    Q[/<word(:expected(-> $candidate = 42 { $candidate == 42 }))>/].AST;
my $default_signature_gist = $default_signature_ast.gist;
ok $default_signature_gist.contains('default => RakuAST::IntLiteral.new(42)'),
    'a defaulted signature keeps its default expression';
ok $default_signature_gist.contains('name => "\\$candidate"'),
    'a defaulted signature keeps its parameter target';
ok EVAL($default_signature_ast) ~~ Regex,
    'a source defaulted-signature regex lowers successfully';

my $default_parameter = RakuAST::Parameter.new(
    target => RakuAST::ParameterTarget::Var.new(name => '$candidate'),
    default => RakuAST::IntLiteral.new(42),
);
my $default_statements = RakuAST::StatementList.new;
$default_statements.add-statement(
    RakuAST::Statement::Expression.new(
        expression => RakuAST::Var::Lexical.new('$candidate'),
    )
);
my $default_pointy = RakuAST::PointyBlock.new(
    signature => RakuAST::Signature.new(parameters => [$default_parameter]),
    body => RakuAST::Blockoid.new($default_statements),
);
my &default_callable = EVAL($default_pointy);
is &default_callable(), 42,
    'a constructed defaulted pointy block supplies its default';
is &default_callable(7), 7,
    'a constructed defaulted pointy block accepts an explicit argument';

my $default_pair = RakuAST::ColonPair::Value.new(
    key => 'expected',
    value => RakuAST::Circumfix::Parentheses.new(
        RakuAST::SemiList.new(
            RakuAST::Statement::Expression.new(expression => $default_pointy),
        ),
    ),
);
my $default_constructed_ast = RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::Named::Args.new(
        name => RakuAST::Name.from-identifier('word'),
        args => RakuAST::ArgList.new($default_pair),
        capturing => True,
    ),
);
ok EVAL($default_constructed_ast) ~~ Regex,
    'a hand-built defaulted-signature regex lowers through the matcher';

my $default_value = 42;
grammar GDynamicDefaultedSignatureColonPairArgument {
    token TOP { <word(:expected(-> $candidate = $default_value { $candidate == 42 }))> };
    token word(:$expected) { <.alpha> <?{ $expected() }> }
}
ok GDynamicDefaultedSignatureColonPairArgument.parse('a').defined,
    'a defaulted signature reaches the named subrule without an argument';
$default_value = 41;
ok !GDynamicDefaultedSignatureColonPairArgument.parse('a').defined,
    'a defaulted signature keeps its outer lexical dynamic';
