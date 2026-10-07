use v6;
use Test;

# A `<...>` word list keeps its raw text in `.AST` (ADR-12199, #12199): rakudo
# writes `<a b  c>` as `QuotedString(processors => <words val>, segments =>
# ("a b  c",))`, whitespace and all, where the parser's own tree is a list of
# three words that cannot tell it from `'a', 'b', 'c'`. A single numeric
# literal (`<1/2>`) is a number term, not a quote.
# This file passes under BOTH mutsu and raku, so raku is the oracle.

plan 31;

sub quote-of(Str $source) {
    $source.AST.statements[0].expression;
}

sub words-of($quote) {
    ($quote.processors.join(' '), $quote.segments.map(*.value).join('|'));
}

# --- read direction: the raw text, node by node ---
{
    my $q = quote-of('<a b  c>;');
    isa-ok $q, RakuAST::QuotedString, 'a word list is a QuotedString';
    is-deeply $q.processors.List, <words val>, 'with the words and val processors';
    is $q.segments.elems, 1, 'and one segment';
    isa-ok $q.segments[0], RakuAST::StrLiteral, 'a string literal';
    is $q.segments[0].value, 'a b  c', 'holding the text with its double space';
}

is-deeply words-of(quote-of('<a>;')), ('words val', 'a'), 'one word is still a word quote';
is-deeply words-of(quote-of('< a b >;')), ('words val', ' a b '), 'the padding is kept';
is-deeply words-of(quote-of("<a\n  b>;")), ('words val', "a\n  b"), 'a newline and its indent are kept';
is-deeply words-of(quote-of('<1 2 3>;')), ('words val', '1 2 3'), 'numbers are quoted text too';
is-deeply words-of(quote-of('<42>;')), ('words val', '42'), 'a single number word is a word quote';

isa-ok quote-of('<1/2>;'), RakuAST::RatLiteral, '<1/2> is a Rat literal, not a quote';
isa-ok quote-of('<1+2i>;'), RakuAST::ComplexLiteral, '<1+2i> is a Complex literal, not a quote';
isa-ok quote-of('< 1/2 >;'), RakuAST::QuotedString, 'padding makes it a quote again';

# --- the node inside its surroundings ---
{
    my $init = quote-of('my @a = <a b  c>;').initializer.expression;
    isa-ok $init, RakuAST::QuotedString, 'the initializer of a declaration';
    is $init.segments[0].value, 'a b  c', 'keeps the text';
}

{
    my $for = 'for <a b> { }'.AST.statements[0];
    isa-ok $for.source, RakuAST::QuotedString, 'the source of a for loop';
    is $for.source.segments[0].value, 'a b', 'keeps the text';
}

{
    my $call = quote-of('<a b  c>.elems;');
    isa-ok $call, RakuAST::ApplyPostfix, 'a postfix call';
    isa-ok $call.operand, RakuAST::QuotedString, 'on a word quote';
    is $call.operand.segments[0].value, 'a b  c', 'with the text';
}

# --- a `handles` clause reads the same term ---
{
    my $class = quote-of('class C { has $.x handles <a b> }');
    my $has = $class.body.body.statement-list.statements[0].expression;
    my $handles = $has.traits[0];
    isa-ok $handles, RakuAST::Trait::Handles, 'the clause is a Handles trait';
    isa-ok $handles.term, RakuAST::QuotedString, 'over a word quote';
    is $handles.term.segments[0].value, 'a b', 'with the written words';
    is-deeply $handles.term.processors.List, <words val>, 'and the words processors';
}

# --- the whole text, against rakudo's own ---
is Q[my @a = <a b  c>;].AST.gist, q:to/END/.chomp, 'my @a = <a b  c>;';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
          sigil       => "\@",
          desigilname => RakuAST::Name.from-identifier("a"),
          initializer => RakuAST::Initializer::Assign.new(
            RakuAST::QuotedString.new(
              processors => <words val>,
              segments   => (
                RakuAST::StrLiteral.new("a b  c"),
              )
            )
          )
        )
      )
    )
    END

# --- write direction: a hand-built word quote still evaluates as a list ---
{
    my $ast = RakuAST::QuotedString.new(
        processors => <words val>,
        segments   => (RakuAST::StrLiteral.new('a b  c'),),
    );
    is-deeply $ast.EVAL.List, ('a', 'b', 'c'), 'EVAL of a hand-built word quote splits the words';
}

# --- semantics: asking for the AST changes nothing about the program ---
{
    my @w = <a b  c>;
    'my @x = <a b  c>;'.AST;
    is-deeply @w, ['a', 'b', 'c'], 'a word list is still a list of its words';
    is-deeply <a  b>.List, ('a', 'b'), 'whitespace between words is not a word';
    ok <1 2>[0] ~~ IntStr, 'a number word is still an allomorph';
    is (for <a b> { .uc }).join, 'AB', 'a word list is still a literal list for `for`';
    is <a b c>.elems, 3, 'and still has its elements';
}
