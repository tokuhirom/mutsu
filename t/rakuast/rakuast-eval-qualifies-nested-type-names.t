use Test;
use experimental :rakuast;

# A bare type name in a type position takes the package it is written in:
# `my Inner $i` inside `class Outer` names `Outer::Inner`. The EVAL($ast) path
# qualifies the lowered tree as an ordinary parse does (checked against rakudo).

plan 4;

my $ast = Q|class Outer {
    class Inner { has $.n }
    method mk { my Inner $i = Inner.new(n => 7); $i.n }
    method typed(Inner $x) { $x.n }
    method name { Inner.^name }
}
Outer.mk|.AST;

is EVAL($ast), 7, 'a typed lexical accepts the nested class instance';

my $ast2 = Q|class Outer2 {
    class Inner2 { has $.n }
    method typed(Inner2 $x) { $x.n }
}
Outer2.typed(Outer2::Inner2.new(n => 9))|.AST;
is EVAL($ast2), 9, 'a parameter typed by the nested class accepts it';

my $ast3 = Q|class Outer3 { class In3 { }; method name { In3.^name } }
Outer3.name|.AST;
is EVAL($ast3), 'Outer3::In3', 'the nested class keeps its qualified name';

my $ast4 = Q|class Outer4 { class In4 { }; method bad { my In4 $x = 42 } }
Outer4.bad|.AST;
throws-like { EVAL($ast4) }, X::TypeCheck::Assignment, 'a non-matching value is still rejected';

