use Test;

# `method NAME handles TERM { ... }` in RakuAST, measured on rakudo 2026.09: a
# `Method` whose `traits` hold a `Trait::Handles` over the written term (a
# `QuotedString(<words val>)` for `<a b>`, `Term::Whatever` for `*`, the string
# for `'a'`). EVAL of the tree installs the same forwarders as the parsed
# declaration.

plan 13;

sub methods($src) {
    $src.AST.statements.head.expression.body.body.statement-list.statements.map(*.expression)
}

# --- the tree
{
    my @m = methods(Q[class C { method m() handles <a b> { 1 }; method w handles * { 2 }; method s handles 'x' { 3 } }]);
    is @m[0].traits.map(*.^name).join(','), 'RakuAST::Trait::Handles', 'a method keeps one Trait::Handles';
    isa-ok @m[0].traits.head.term, RakuAST::QuotedString, '`handles <a b>` is a word quote';
    is @m[0].traits.head.term.processors.join(','), 'words,val', '... with its processors';
    isa-ok @m[1].traits.head.term, RakuAST::Term::Whatever, '`handles *` is a Whatever';
    isa-ok @m[2].traits.head.term, RakuAST::QuotedString, '`handles \'x\'` is a string';
}
{
    my @m = methods(Q[class C2 { method m handles<a> is rw { 1 } }]);
    is @m[0].traits.map(*.^name).join(','), 'RakuAST::Trait::Handles,RakuAST::Trait::Is',
        'traits keep their written order';
}

# --- EVAL of the tree
# Each tree declares its own helper class: two EVALs of the same package
# declaration are a redeclaration in rakudo.
sub with-helper($name, $src) {
    (Q[class HELPER { method greet($x) { "hi $x" }; method other { 'o' } }; ] ~ $src).subst('HELPER', $name, :g)
}
is EVAL(with-helper('MhH1', Q[class MhA { method h() handles <greet other> { HELPER.new } }; MhA.new.greet('z') ~ MhA.new.other]).AST),
    'hi zo', 'a word-list `handles` forwards each name';
is EVAL(Q[class MhB { method inner() handles * { 'hello' } }; MhB.new.uc ~ MhB.new.flip].AST),
    'HELLOolleh', '`handles *` forwards every unknown method';
is EVAL(with-helper('MhH2', Q[class MhC { method h handles 'greet' { HELPER.new } }; MhC.new.greet('y')]).AST),
    'hi y', 'a single quoted name';
is EVAL(with-helper('MhH3', Q[class MhD { method h handles<greet> { HELPER.new } }; MhD.new.h.^name]).AST),
    'MhH3', 'the handles-carrying method itself is declared';

# --- semantics, as parsed
{
    class MhP { method greet($x) { "hey $x" } }
    class MhE { method h() handles <greet> { MhP.new } }
    is MhE.new.greet('q'), 'hey q', 'the parsed declaration forwards';
    class MhW { method inner() handles * { 'abc' } }
    is MhW.new.chars, 3, '`handles *` on a parsed method';
    is MhW.new.inner, 'abc', 'and the method answers its own value';
}
