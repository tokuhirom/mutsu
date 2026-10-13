use Test;

# Constant colonpairs written as a statement in RakuAST, measured on rakudo
# 2026.09: `:a(7)`, `:a`, `:!a` and `:7a` stay the written `ColonPair::Value`,
# `ColonPair::True`, `ColonPair::False` and `ColonPair::Number`, including in a
# string's `{...}` block and in an `@{...}` composer, where the parser folds the
# constant pair (keeping its source text for the sink warning).

plan 16;

sub expr($src) { $src.AST.statements.head.expression }
sub block-expr($src) {
    expr($src).segments.head.body.statement-list.statements.head.expression
}

# --- the tree
isa-ok block-expr(Q["{:a(7)}"]), RakuAST::ColonPair::Value, '`"{:a(7)}"` holds a ColonPair::Value';
is block-expr(Q["{:a(7)}"]).key, 'a', '... with its key';
isa-ok block-expr(Q["{:a}"]), RakuAST::ColonPair::True, '`"{:a}"` holds a ColonPair::True';
isa-ok block-expr(Q["{:!a}"]), RakuAST::ColonPair::False, '`"{:!a}"` holds a ColonPair::False';
isa-ok expr(Q[:a(7)]), RakuAST::ColonPair::Value, 'a statement `:a(7)`';
isa-ok expr(Q[:7a]), RakuAST::ColonPair::Number, 'a statement `:7a` is a ColonPair::Number';
{
    my $n = expr(Q[:7a]);
    is $n.key, 'a', '... with its key';
    isa-ok $n.value, RakuAST::IntLiteral, '... and its number';
}
isa-ok Q[sub f(*%h) { %h }; f(:3rd)].AST.statements[1].expression.args.args.head,
    RakuAST::ColonPair::Number, '`:3rd` as an argument too';
isa-ok expr(Q[@{:a}]).target.expression, RakuAST::ColonPair::True,
    '`@{:a}` composes a ColonPair::True';

# --- EVAL of the tree
is Q["{:a(7)}"].AST.EVAL, "a\t7", 'a string block answers its pair';
is Q["{:a}"].AST.EVAL, "a\tTrue", 'a True pair';
is Q[@{:a, :b}].AST.EVAL.elems, 2, 'an @{...} composer of pairs';
is-deeply Q[(:7a)].AST.EVAL, (a => 7), 'a numeric colonpair';
is Q[sub g(*%h) { %h.sort.map({ "{.key}={.value}" }).join(',') }; g(:7a, :3rd)].AST.EVAL,
    'a=7,rd=3', 'numeric colonpairs are named arguments';

# --- semantics, as parsed
is "{:a(7)}", "a\t7", 'the parsed string block';
