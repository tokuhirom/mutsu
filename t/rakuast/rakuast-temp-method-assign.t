use Test;

# A temp method lvalue is represented by the ordinary RakuAST prefix and
# assignment nodes. The lowering returns to the parser's save/restore form.

plan 12;

sub method-exprs($src) {
    $src.AST.statements[0].expression.body.body.statement-list.statements[1].expression.body.statement-list.statements.map(*.expression)
}

{
    my @e = method-exprs(Q[class C1 { has $.n is rw = 1; method set { temp $.n = 4; temp self.n = 5; temp $!n = 6 } }]);
    isa-ok @e[0], RakuAST::ApplyInfix, 'temp $.n = is an ApplyInfix';
    isa-ok @e[0].left, RakuAST::ApplyPrefix, 'with a temp prefix';
    is @e[0].left.prefix.operator, 'temp', 'and the temp operator';
    isa-ok @e[0].left.operand, RakuAST::Var::Attribute::Public, 'over the public attribute';
    like @e[0].infix.gist, /':item'/, 'attribute assignment is itemized';
    isa-ok @e[1].left.operand, RakuAST::ApplyPostfix, 'temp self.n uses a method call';
    isa-ok @e[1].left.operand.postfix, RakuAST::Call::Method, 'with the method node';
    isa-ok @e[2].left.operand, RakuAST::Var::Attribute, 'private attribute spelling stays distinct';
}

{
    my @e = method-exprs(Q[class C2 { has $.n is rw = 1; method add { temp $.n += 3; $.n } }]);
    isa-ok @e[0].infix, RakuAST::MetaInfix::Assign, 'compound assignment keeps its meta-infix';
    is @e[0].infix.infix.gist, 'RakuAST::Infix.new("+")', 'the compound operator is preserved';
}

is EVAL(Q[class C3 { has $.n is rw = 1; method set { temp $.n = 4; $.n } }; my $c = C3.new; my $inside = $c.set; "$inside|{$c.n}"].AST),
    '4|1', 'a temporary attribute assignment restores the value';
is EVAL(Q[class C4 { has $.n is rw = 1; method add { temp $.n += 3; $.n } }; my $c = C4.new; my $inside = $c.add; "$inside|{$c.n}"].AST),
    '4|1', 'a temporary compound assignment restores the value';
