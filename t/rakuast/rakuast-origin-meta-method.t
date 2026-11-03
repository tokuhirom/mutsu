use Test;
use experimental :rakuast;

# `.^name` is a `Call::MetaMethod`: it and its `ArgList` carry a range
# (the call from its name to its end; the argument list empty right after the
# name, just inside the parentheses, or from the first argument to the last),
# and the empty `ArgList` is visited (#15780). Ranges measured on rakudo
# 2026.09.

plan 4;

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

is nodes('my $n; $n.^name;').join(', '),
    'Statement::Expression 0..5, VarDeclaration::Simple 0..5, Statement::Expression 7..15, ApplyPostfix 7..15, Var::Lexical 7..9, Call::MetaMethod 11..15, ArgList 15..15',
    'no parentheses: an empty argument list right after the name';

is nodes('my $n; $n.^name();').join(', '),
    'Statement::Expression 0..5, VarDeclaration::Simple 0..5, Statement::Expression 7..17, ApplyPostfix 7..17, Var::Lexical 7..9, Call::MetaMethod 11..17, ArgList 16..16',
    'empty parentheses';

is nodes('my $n; $n.^methods(:local);').join(', '),
    'Statement::Expression 0..5, VarDeclaration::Simple 0..5, Statement::Expression 7..26, ApplyPostfix 7..26, Var::Lexical 7..9, Call::MetaMethod 11..26, ArgList 19..25, ColonPair::True 19..25',
    'a colonpair argument';

is nodes('my $n; $n.^can("x").so;').join(', '),
    'Statement::Expression 0..5, VarDeclaration::Simple 0..5, Statement::Expression 7..22, ApplyPostfix 7..22, ApplyPostfix 7..19, Var::Lexical 7..9, Call::MetaMethod 11..19, ArgList 15..18, QuotedString 15..18, StrLiteral 16..17, Call::Method 20..22, Name 20..22, ArgList 22..22',
    'a metamethod in the middle of a chain';

