use Test;

# The colonpairs written as call arguments or list items carry the ranges
# Rakudo gives them (ADR-10723 Stage 2, #15780): the pair, its value (the
# variable of `:$x`, the number of `:5h`, the word quote of `:e<f g>`, the
# parentheses of `:b(3)`) and the `ArgList` around them. Ranges measured on
# rakudo 2026.09.

plan 4;

# Every visited node of `$src` below the first colonpair, with its range.
sub pairs(Str $src) {
    my @r;
    my $inside = False;
    sub walk($node) {
        my $name = $node.^name.subst('RakuAST::', '');
        my $was = $inside;
        $inside = True if $name.starts-with('ColonPair');
        @r.push: $name ~ ' ' ~ ($node.origin andthen "{.from}..{.to}" orelse '-') if $inside;
        $node.visit-children(&walk);
        $inside = $was;
    }
    $src.AST.visit-children(&walk);
    @r.join(', ')
}

my $pre = 'sub foo(|c) {}; my $x = 1; ';

is pairs($pre ~ 'foo(:a, :$x);'),
    'ColonPair::True 31..33, ColonPair::Variable 35..38, Var::Lexical 36..38',
    'a bare pair and a variable pair as arguments';
is pairs($pre ~ 'foo(:!d, :5h);'),
    'ColonPair::False 31..34, ColonPair::Number 36..39, IntLiteral 37..38',
    'a negated pair and a numeric pair';
is pairs($pre ~ 'foo(:e<f g>);'),
    'ColonPair::Value 31..38, QuotedString 33..38, StrLiteral 34..37',
    'a word-quote value';
is pairs($pre ~ 'foo(:b(3));'),
    'ColonPair::Value 31..36, Circumfix::Parentheses 33..36, SemiList 34..35, '
    ~ 'Statement::Expression 34..35, IntLiteral 34..35',
    'a parenthesized value';
