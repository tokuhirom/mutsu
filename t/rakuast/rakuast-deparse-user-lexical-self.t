use Test;
plan 5;
is Q[my $self = 1; $self = 2; say $self].AST.DEPARSE.chomp, "my \$self = 1;\n\$self = 2;\nsay \$self", 'a lexical named $self';
is Q[sub f($self is rw) { $self = 1 }].AST.DEPARSE.chomp.subst(/\s+/, ' ', :g), 'sub f ($self is rw) { $self = 1 }', 'a $self parameter';
is EVAL(Q[my $self = 5; $self = $self + 1; $self].AST), 6, 'EVAL of a $self lexical';
is EVAL(Q[sub f($self) { $self + 1 }; f(4)].AST), 5, 'EVAL of a $self parameter';
is Q[my $self = 1; $self].AST.statements[1].expression.name, '$self', 'Var::Lexical name';
