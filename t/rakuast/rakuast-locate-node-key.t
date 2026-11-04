use Test;

# `locate-node($pos, $to, :key)` answers the nearest enclosing statement-like
# node of the deepest node at the position (#14916). Results measured on rakudo
# 2026.09.

plan 8;

my $src = Q[if 1 { say 5; say 6 } else { say 7 }; sub f($a) { return $a + 1 }];
my $ast = $src.AST;

sub key-of(Int $p) {
    my $k = $ast.locate-node($p, $p + 1, :key);
    $k.^name.subst('RakuAST::', '') ~ ' ' ~ ($k.origin andthen "{.from}-{.to}" orelse '-')
}

is key-of(12), 'StatementList 7-19', 'a statement list is its own key';
is key-of(4), 'Statement::If 0-36', 'a statement is its own key';
is key-of(29), 'Statement::Expression 29-34', 'a name inside a statement';
is key-of(45), 'Sub 38-65', 'a parameter variable climbs to the sub';
is key-of(60), 'Statement::Expression 50-63', 'an operator inside a statement';
is key-of(18), 'Statement::Expression 14-19', 'a literal inside a statement';
is key-of(20), 'Blockoid 5-21', 'a blockoid is its own key';
is $ast.locate-node(5, 6).^name, 'RakuAST::Blockoid', 'without :key the deepest node';
