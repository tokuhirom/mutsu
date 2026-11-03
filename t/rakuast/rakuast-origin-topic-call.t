use Test;
use experimental :rakuast;

# A topic call (`.name`, `.name(args)`) is a `Term::TopicCall` over a
# `Call::Method`; the call, its `Name` and its `ArgList` carry the ranges
# rakudo gives them, also as an argument, inside a block and inside an
# interpolated `{...}` (#15780). Ranges measured on rakudo 2026.09.

plan 5;

# Each visited node of `$src`, with its range.
sub nodes(Str $src) {
    my @r;
    sub walk($node) {
        @r.push: $node.^name.subst('RakuAST::', '') ~ ' '
            ~ ($node.origin andthen "{.from}..{.to}" orelse '-');
        $node.visit-children(&walk);
    }
    $src.AST.visit-children(&walk);
    @r
}

is nodes('my $n; say .abs with $n;').join(', '),
    'Statement::Expression 0..5, VarDeclaration::Simple 0..5, Statement::Expression 7..23, Call::Name::WithoutParentheses 7..15, Name 7..10, ArgList 11..15, Term::TopicCall 11..15, Call::Method 12..15, Name 12..15, ArgList 15..15, StatementModifier::With 16..23, Var::Lexical 21..23',
    'a topic call as an argument';

is nodes('for 1 { .say }').join(', '),
    'Statement::For 0..14, IntLiteral 4..5, Block 6..14, Blockoid 6..14, StatementList 8..12, Statement::Expression 8..12, Term::TopicCall 8..12, Call::Method 9..12, Name 9..12, ArgList 12..12',
    'a topic call as a statement in a block';

is nodes('my $n; say "x {.abs}" with $n;').join(', '),
    'Statement::Expression 0..5, VarDeclaration::Simple 0..5, Statement::Expression 7..29, Call::Name::WithoutParentheses 7..21, Name 7..10, ArgList 11..21, QuotedString 11..21, StrLiteral 12..14, Block 14..20, Blockoid 14..20, StatementList 15..19, Statement::Expression 15..19, Term::TopicCall 15..19, Call::Method 16..19, Name 16..19, ArgList 19..19, StatementModifier::With 22..29, Var::Lexical 27..29',
    'a topic call in an interpolated block';

is nodes('my $n; say .substr(1, 2) with $n;').join(', '),
    'Statement::Expression 0..5, VarDeclaration::Simple 0..5, Statement::Expression 7..32, Call::Name::WithoutParentheses 7..24, Name 7..10, ArgList 11..24, Term::TopicCall 11..24, Call::Method 12..24, Name 12..18, ArgList 19..23, IntLiteral 19..20, IntLiteral 22..23, StatementModifier::With 25..32, Var::Lexical 30..32',
    'a topic call with arguments';

is nodes('.uc.lc').join(', '),
    'Statement::Expression 0..6, ApplyPostfix 0..6, Term::TopicCall 0..3, Call::Method 1..3, Name 1..3, ArgList 3..3, Call::Method 4..6, Name 4..6, ArgList 6..6',
    'a chained topic call';

