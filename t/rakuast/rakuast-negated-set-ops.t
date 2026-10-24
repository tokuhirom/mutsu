use Test;

# Negated set operators and left-exclusive sequences are rendered as the
# operator the source wrote, not as the parser's desugaring (#14912). Every
# expectation here was measured against rakudo 2026.09.

plan 28;

sub expr(Str $source, Int $index) { $source.AST.statements[$index].expression.raku }

# --- read direction ------------------------------------------------------

for <∉ ∌ ⊄ ⊅ ⊈ ⊉> -> $glyph {
    my $raku = expr(Q[my $a; my $b; $a GLYPH $b].subst('GLYPH', $glyph), 2);
    ok $raku.contains(qq[infix => RakuAST::Infix.new("$glyph")]),
        "$glyph is its own infix";
    nok $raku.contains('ApplyPrefix'), "$glyph has no prefix negation";
}

for <(elem) ∈> -> $rel {
    my $raku = expr(Q[my $a; my $b; $a !REL $b].subst('REL', $rel), 2);
    ok $raku.contains('RakuAST::MetaInfix::Negate.new')
        && $raku.contains(qq[RakuAST::Infix.new("$rel")]),
        "!$rel is the negation meta infix over $rel";
}

for <^... ^...^ ^… ^…^> -> $seq {
    my $raku = expr(Q[my $a; my $b; $a SEQ $b].subst('SEQ', $seq), 2);
    ok $raku.starts-with('RakuAST::ApplyListInfix.new(')
        && $raku.contains(qq[infix    => RakuAST::Infix.new("$seq")])
        && !$raku.contains('skip'),
        "$seq is one list infix over both operands";
}

my $chain = expr(Q[my $a; my $b; my $c; $a ^... $b ^... $c], 3);
is $chain.comb('RakuAST::Var::Lexical').elems, 3,
    'a chain of left-exclusive sequences is one flat operand list';

# --- semantics -----------------------------------------------------------

is-deeply (1 ^... 5).list, (2, 3, 4, 5), '^... drops the seed';
is-deeply (1 ^…^ 5).list, (2, 3, 4), '^…^ drops seed and end';
is-deeply (5 ^… 1).list, (4, 3, 2, 1), '^… counts down';
is (1 ∉ (2, 3)), True, '∉ on a non-member';
is (1 !(elem) (1, 2)), False, '!(elem) on a member';
is (1 !∈ (2,)), True, '!∈ on a non-member';

# --- write direction -----------------------------------------------------

my $negated = Q[my $a = 1; my $b = (2, 3); $a ∉ $b].AST;
is EVAL($negated), True, 'the round-tripped ∉ evaluates';
my $meta = Q[my $a = 1; my $b = (1, 2); $a !(elem) $b].AST;
is EVAL($meta), False, 'the round-tripped !(elem) evaluates';
my $seq = Q[my $a = 1; my $b = 5; $a ^... $b].AST;
is-deeply EVAL($seq).list, (2, 3, 4, 5), 'the round-tripped ^... evaluates';
