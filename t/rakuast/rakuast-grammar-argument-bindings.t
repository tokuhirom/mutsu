use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

my $tree = Q[class BoundPick { has $.want }; /<Target::word(BoundPick.new(want => 'ab'))>/].AST;
my $call = $tree.statements[1].expression.body.args.args[0];
isa-ok $call, RakuAST::ApplyPostfix, 'an object argument remains a method application';
isa-ok $call.postfix.args.args[0], RakuAST::FatArrow, 'a nested named argument remains a FatArrow';
is $call.postfix.args.args[0].key, 'want', 'the nested named key stays unquoted';

is EVAL(Q[
    class ObjectArg { has $.want }
    grammar ObjectGrammar {
        regex TOP { <word(ObjectArg.new(want => 'ab'))> c }
        regex word($object) { (\w+) <?{ ~$0 eq $object.want }> }
    }
    ~ObjectGrammar.parse('abc')<word>
].AST), 'ab', 'an object argument keeps identity across backtracking after AST lowering';

my @named-seen;
grammar NamedCapture {
    token TOP { <word> <?{ so $<word> }> <.record: "seen $<word>"> }
    token word { \w+ }
    method record($text) { @named-seen.push($text); self }
}
NamedCapture.parse('abc');
is @named-seen.join(','), 'seen abc', 'argument interpolation reads the live named capture';

my @positional-seen;
grammar PositionalCapture {
    token TOP { (\w+) <?{ so $0 }> <.record: "seen $0"> }
    method record($text) { @positional-seen.push($text); self }
}
PositionalCapture.parse('abc');
is @positional-seen.join(','), 'seen abc', 'argument interpolation reads the live positional capture';

is EVAL(Q[
    my $calls = 0;
    sub argument() { $calls++; 'a' }
    grammar SkippedArgument {
        token TOP { a || <word(argument())> }
        token word($text) { $text }
    }
    SkippedArgument.parse('a');
    $calls
].AST), 0, 'a skipped branch does not evaluate its argument while rendering the rule';

lives-ok {
    EVAL(Q[
        sub argument() { die 'unentered argument' }
        grammar SkippedException {
            token TOP { a || <word(argument())> }
            token word($text) { $text }
        }
        SkippedException.parse('a')
    ].AST)
}, 'an exception in an unentered argument does not escape the parse';

is EVAL(Q[
    my $calls = 0;
    sub argument() { $calls++; 'a' }
    grammar EnteredArgument {
        token TOP { <word(argument())> }
        token word($text) { $text }
    }
    EnteredArgument.parse('a');
    $calls
].AST), 1, 'an entered argument runs once at its call site';

ok EVAL(Q[
    grammar DefaultArgument {
        token TOP { <middle> }
        token middle($text = 'ab') { <word($text ~ 'c')> }
        token word($text) { $text }
    }
    so DefaultArgument.parse('abc')
].AST), 'a defaulted enclosing parameter survives nested argument reconstruction';

is EVAL(Q[
    grammar PositionalPairArgument {
        token TOP { <word(('want' => 'ab'))> }
        token word($pair) { ab <?{ $pair.key eq 'want' && $pair.value eq 'ab' }> }
    }
    ~PositionalPairArgument.parse('ab')
].AST), 'ab', 'a grouped quoted-key Pair stays positional';

is EVAL(Q[
    grammar NamedPairArgument {
        token TOP { <word(want => 'ab')> }
        token word(:$want) { $want }
    }
    ~NamedPairArgument.parse('ab')
].AST), 'ab', 'an ungrouped bareword-key Pair stays named';

is EVAL(Q[
    class PairPayload { has $.want }
    grammar OpaquePairArgument {
        token TOP { <word(('payload' => PairPayload.new(want => 'ab')))> }
        token word($pair) { ab <?{ $pair.value.want eq 'ab' }> }
    }
    ~OpaquePairArgument.parse('ab')
].AST), 'ab', 'a positional Pair containing an object is retained in the binding window';

is EVAL(Q[
    grammar ComputedName {
        token TOP { <word> }
        token word($name = 'alpha') { <::($name)> }
        token alpha { a }
    }
    ComputedName.parse('a')<word>.hash.keys.elems
].AST), 0, 'specializing a defaulted computed rule name does not make a named capture';

my $assertion = RakuAST::Regex::Assertion::Named::Args.new(
        name => RakuAST::Name.from-identifier('word'),
        args => RakuAST::ArgList.new(RakuAST::QuotedString.new(
            segments => (RakuAST::StrLiteral.new('ab'),)
        )),
        capturing => True
);
my $constructed = RakuAST::Grammar.new(
    name => RakuAST::Name.from-identifier('ConstructedArgumentBindings'),
    body => RakuAST::Block.new(body => RakuAST::Blockoid.new(
        RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(expression => RakuAST::TokenDeclaration.new(
                name => RakuAST::Name.from-identifier('TOP'),
                body => RakuAST::Regex::WithWhitespace.new($assertion)
            )),
            RakuAST::Statement::Expression.new(expression => RakuAST::TokenDeclaration.new(
                name => RakuAST::Name.from-identifier('word'),
                signature => RakuAST::Signature.new(parameters => (
                    RakuAST::Parameter.new(
                        type => RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Any')),
                        target => RakuAST::ParameterTarget::Var.new(name => '$text'),
                        optional => False
                    ),
                )),
                body => RakuAST::Regex::WithWhitespace.new(
                    RakuAST::Regex::Interpolation.new(
                        sequential => False,
                        var => RakuAST::Var::Lexical.new('$text')
                    )
                )
            ))
        )
    ))
);
my $grammar = EVAL(RakuAST::StatementList.new(
    RakuAST::Statement::Expression.new(expression => $constructed)
));
ok $grammar.parse('ab'), 'a hand-built argumented assertion uses the same compiled call path';
nok $grammar.parse('cd'), 'the hand-built assertion retains argument constraints';

done-testing;
