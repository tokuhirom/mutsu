use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# A statement with modifiers is one node: the statement as written, its
# `condition-modifier` and its `loop-modifier`, each placed where it was
# written (ADR-10723 Stage 2). Ranges measured on rakudo 2026.09; this file
# passes under both mutsu and raku.

plan 16;

sub range($node) { with $node.origin { "{.from}..{.to}" } else { 'none' } }
sub shape(Str $src) {
    my $statement = $src.AST.statements[0];
    my @parts;
    $statement.visit-children(-> $c { @parts.push: $c.^name.subst('RakuAST::', '') ~ '@' ~ range($c) });
    range($statement) ~ ' ' ~ @parts.join(' ')
}

is shape(Q[say 1 if $*x for 1, 2;]),
    '0..21 Call::Name::WithoutParentheses@0..5 StatementModifier::If@6..12 StatementModifier::For@13..21',
    'a condition and a loop modifier';
is shape(Q[sub a { 5 } given 3;]), '0..19 Sub@0..11 StatementModifier::Given@12..19',
    'a modifier on a routine declaration';
is shape(Q[my $y = 1 if $*x;]), '0..16 VarDeclaration::Simple@0..9 StatementModifier::If@10..16',
    'a modifier on a declaration';
is shape(Q[.say for 1, 2;]), '0..13 Term::TopicCall@0..4 StatementModifier::For@5..13',
    'a for modifier';
is shape("say 1 #c\n if \$*x;"),
    '0..16 Call::Name::WithoutParentheses@0..8 StatementModifier::If@10..16',
    'a comment before the keyword is part of the statement';
is shape(Q[(say 1 if $*x).say;]), '0..18 ApplyPostfix@0..18',
    'a parenthesised modified statement is an operand';

# The tree lowers to what the parsed program does.
sub run(Str $src) { EVAL($src.AST) }
is run(Q[sub a { 5 } given 3; a()]), 5, 'the routine is declared under its modifier';
is run(Q[my $y = 1 if 0; $y.defined]), False, 'the declaration is unconditional, its initializer is not';
is run(Q[my @o; @o.push($_) if $_ %% 2 for 1..6; @o.join(',')]), '2,4,6', 'the condition applies inside the loop';
is run(Q[my $n = 0; $n++ while $n < 3; $n]), 3, 'a while modifier loops';
is run(Q[my $n = 5; $n-- until $n == 2; $n]), 2, 'an until modifier loops';
is run(Q[my $r; $r = $_ given 7; $r]), 7, 'a given modifier topicalizes';
is run(Q[my $r = (10 with 3); $r]), 10, 'a with modifier on an expression keeps its value';
is run(Q[my $r = 1; $r = 2 without 3; $r]), 1, 'a without modifier on a defined topic does not run';
is run(Q[my @o; given 4 { @o.push('four') when 4; @o.push('after') }; @o.join(',')]), 'four,after',
    'a when modifier runs its statement on a match';

# A hand-built modified statement lowers the same way.
my $built = RakuAST::StatementList.new(
    RakuAST::Statement::Expression.new(
        expression         => RakuAST::IntLiteral.new(42),
        condition-modifier => RakuAST::StatementModifier::If.new(RakuAST::IntLiteral.new(1)),
    )
);
is EVAL($built), 42, 'a hand-built condition modifier';
