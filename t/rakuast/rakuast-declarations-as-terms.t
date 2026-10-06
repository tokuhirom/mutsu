use Test;

# Declarations in RakuAST, measured on rakudo 2026.09. The parser expands a
# few declaration forms into several statements of its own (a sigilless
# declaration is a bind plus a marker, a declaration under a statement
# modifier is a declaration plus a gated assignment, `.=` is a read, a call
# and an assignment); the tree is the one node rakudo builds:
#
# - `my \x = 5` / `my \y := 5` are a `VarDeclaration::Term`;
# - `my $x = 1 if COND` keeps the declaration and the modifier together on
#   one `Statement::Expression`;
# - `$s .= uc` is an `ApplyDottyInfix` with a `DottyInfix::CallAssign`;
# - a declaration is also a term: `my $v = my $w = 3`, `say (my $z = 4)`.
#
# Lowering restores the parser's expansion, so EVAL of `.AST` computes what
# the parsed program does.

plan 38;

sub stmts($src) { $src.AST.statements }
sub exprs($src) { stmts($src).map(*.expression) }
sub run($src) { EVAL($src.AST) }

# --- a sigilless declaration is a Term declaration
{
    my @e = exprs(Q[my \x = 5; x]);
    isa-ok @e[0], RakuAST::VarDeclaration::Term, '`my \x = 5` is a VarDeclaration::Term';
    is @e[0].name.canonicalize, 'x', 'with the bare name';
    isa-ok @e[0].initializer, RakuAST::Initializer::Assign, 'assigned';
    isa-ok @e[1], RakuAST::Term::Name, 'and the name is then a term';
    my @b = exprs(Q[my \y := 5; y]);
    isa-ok @b[0], RakuAST::VarDeclaration::Term, '`my \y := 5` is a VarDeclaration::Term';
    isa-ok @b[0].initializer, RakuAST::Initializer::Bind, 'bound';
    like @b[0].gist, /'Initializer::Bind.new(' \s* 'RakuAST::IntLiteral.new(5)'/, 'to the value';
}

# --- a declaration under a statement modifier
{
    my @s = stmts(Q[my $a = 1 if True]);
    is @s.elems, 1, 'a conditional declaration is one statement';
    isa-ok @s[0].expression, RakuAST::VarDeclaration::Simple, 'a variable declaration';
    isa-ok @s[0].condition-modifier, RakuAST::StatementModifier::If, 'under `if`';
    my @u = stmts(Q[my $a = 1 unless False]);
    isa-ok @u[0].condition-modifier, RakuAST::StatementModifier::Unless, '`unless` stays `unless`';
    my @t = stmts(Q[my Int $n = 3 if True]);
    isa-ok @t[0].expression.type, RakuAST::Type::Simple, 'a typed declaration keeps its type';
    isa-ok @t[0].condition-modifier, RakuAST::StatementModifier::If, 'and its modifier';
}

# --- `.=` is a dotty infix
{
    my @e = exprs(Q[my $s = "ab"; $s .= uc]);
    isa-ok @e[1], RakuAST::ApplyDottyInfix, '`$s .= uc` is an ApplyDottyInfix';
    isa-ok @e[1].infix, RakuAST::DottyInfix::CallAssign, 'a CallAssign';
    isa-ok @e[1].right, RakuAST::Call::Method, 'with a method call';
    is @e[1].right.name.canonicalize, 'uc', 'of the right name';
    isa-ok @e[1].left, RakuAST::Var::Lexical, 'on the variable';
    my @a = exprs(Q[my @a = 3,1,2; @a .= sort]);
    isa-ok @a[1], RakuAST::ApplyDottyInfix, 'also on an array';
    my @m = stmts(Q[my $t = "x"; $t .= uc if True]);
    isa-ok @m[1].expression, RakuAST::ApplyDottyInfix, 'under a statement modifier';
    isa-ok @m[1].condition-modifier, RakuAST::StatementModifier::If, 'it keeps the modifier';
}

# --- a declaration is a term
{
    my @e = exprs(Q[my $v = my $w = 3]);
    isa-ok @e[0].initializer.expression, RakuAST::VarDeclaration::Simple, 'the initializer is a declaration';
    my @c = exprs(Q[say (my $z = 4)]);
    isa-ok @c[0], RakuAST::Call::Name::WithoutParentheses, 'a call';
    like @c[0].gist, /'Circumfix::Parentheses' .* 'VarDeclaration::Simple'/, 'on a parenthesized declaration';
}

# --- the round trip computes what the parsed program does
is run(Q[my \x = 5; x + 1]), 6, 'a sigilless assigned declaration';
is run(Q[my \y := 7; y * 2]), 14, 'a sigilless bound declaration';
is run(Q[my $a = 1; $a = 2 if False; $a]), 1, 'a gated assignment does not run when the condition fails';
is run(Q[my $a = 1 if True; $a]), 1, 'a conditional declaration runs when it holds';
is run(Q[my $a = 1 if False; $a.defined.Str]), 'False', 'and declares without initializing otherwise';
is run(Q[my $a = 1 unless True; $a.defined.Str]), 'False', 'an `unless` declaration is gated by the negation';
is run(Q[my $s = "ab"; $s .= uc; $s]), 'AB', '`.=` assigns the call result';
is run(Q[my @a = 3,1,2; @a .= sort; @a.join(",")]), '1,2,3', '`.=` on an array';
is run(Q[my $t = "x"; $t .= uc if False; $t]), 'x', '`.=` under a false modifier does nothing';
is run(Q[my $t = "x"; $t .= uc if True; $t]), 'X', 'and under a true one assigns';
is run(Q[my $x = "a"; $x .= pred; $x.WHAT.^name]), 'Failure', 'a Failure assigned by `.=` is stored, not thrown';
is run(Q[my $v = my $w = 3; $v + $w]), 6, 'a declaration as an initializer';
is run(Q[my $n = (my $z = 4) + 1; $n + $z]), 9, 'a declaration inside an expression';
is run(Q[sub f { my \x = 3; x * x }; f()]), 9, 'a sigilless declaration in a sub';
