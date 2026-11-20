use Test;
use experimental :rakuast;

# Origins and visit order of package declarations, as rakudo 2026.11 has them
# (#15780): a package's body `Block` starts at its first trait and visits the
# package's traits before its `Blockoid`; a role's `RoleBody` visits the
# role's name, its parameterization and its traits; a name's adverbs
# (`E:ver<1>`) are part of the `Name`; a trait's parenthesized argument and
# a `handles` term are placed; a method's empty `()` has a `Signature` range;
# a bare `is export` on a module or package has no argument.

plan 10;

sub walk(Str $src) {
    my @l;
    sub w($n) {
        with $n.origin { @l.push($n.^name.subst('RakuAST::', '') ~ '@' ~ .from ~ '-' ~ .to) }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}

is walk("role R \{ }"),
    "StatementList\@0-10 Statement::Expression\@0-10 Role\@0-10 Name\@5-6 RoleBody\@5-10 Name\@5-6 Blockoid\@7-10 StatementList\@8-8",
    'origins of case 1';
is walk("class C is Int does Positional \{ }"),
    "StatementList\@0-34 Statement::Expression\@0-34 Class\@0-34 Name\@6-7 Block\@8-34 Trait::Is\@8-14 Type::Simple\@11-14 Name\@11-14 Trait::Does\@15-30 Type::Simple\@20-30 Name\@20-30 Blockoid\@31-34 StatementList\@32-32",
    'origins of case 2';
is walk("grammar G is Grammar \{ }"),
    "StatementList\@0-24 Statement::Expression\@0-24 Grammar\@0-24 Name\@8-9 Block\@10-24 Trait::Is\@10-20 Type::Simple\@13-20 Name\@13-20 Blockoid\@21-24 StatementList\@22-22",
    'origins of case 3';
is walk("module M is export \{ }"),
    "StatementList\@0-22 Statement::Expression\@0-22 Module\@0-22 Name\@7-8 Block\@9-22 Trait::Is\@9-18 Name\@12-18 Blockoid\@19-22 StatementList\@20-20",
    'origins of case 4';
is walk("package P is export \{ }"),
    "StatementList\@0-23 Statement::Expression\@0-23 Package\@0-23 Name\@8-9 Block\@10-23 Trait::Is\@10-19 Name\@13-19 Blockoid\@20-23 StatementList\@21-21",
    'origins of case 5';
is walk("class E:ver<1>:auth<zef:x> is Int \{ }"),
    "StatementList\@0-37 Statement::Expression\@0-37 Class\@0-37 Name\@6-26 ColonPair::Value\@7-14 QuotedString\@11-14 StrLiteral\@12-13 ColonPair::Value\@14-26 QuotedString\@19-26 StrLiteral\@20-25 Block\@27-37 Trait::Is\@27-33 Type::Simple\@30-33 Name\@30-33 Blockoid\@34-37 StatementList\@35-35",
    'origins of case 6';
is walk("class L is Int is export \{ }"),
    "StatementList\@0-28 Statement::Expression\@0-28 Class\@0-28 Name\@6-7 Block\@8-28 Trait::Is\@8-14 Type::Simple\@11-14 Name\@11-14 Trait::Is\@15-24 Name\@18-24 Blockoid\@25-28 StatementList\@26-26",
    'origins of case 7';
is walk("class A \{ method h() \{ 1 }; method g() is rw \{ 1 }; method k(\$x) handles <a> \{ 1 }; method i() handles * \{ 2 } }"),
    "StatementList\@0-112 Statement::Expression\@0-112 Class\@0-112 Name\@6-7 Block\@8-112 Blockoid\@8-112 StatementList\@10-110 Statement::Expression\@10-26 Method\@10-26 Name\@17-18 Signature\@19-19 Blockoid\@21-26 StatementList\@23-24 Statement::Expression\@23-24 IntLiteral\@23-24 Statement::Expression\@28-50 Method\@28-50 Name\@35-36 Signature\@37-37 Trait::Is\@39-44 Name\@42-44 Blockoid\@45-50 StatementList\@47-48 Statement::Expression\@47-48 IntLiteral\@47-48 Statement::Expression\@52-82 Method\@52-82 Name\@59-60 Signature\@61-63 Parameter\@61-63 ParameterTarget::Var\@61-63 VarDeclaration::Simple\@61-63 Trait::Handles\@65-76 QuotedString\@73-76 StrLiteral\@74-75 Blockoid\@77-82 StatementList\@79-80 Statement::Expression\@79-80 IntLiteral\@79-80 Statement::Expression\@84-110 Method\@84-110 Name\@91-92 Signature\@93-93 Trait::Handles\@95-104 Term::Whatever\@103-104 Blockoid\@105-110 StatementList\@107-108 Statement::Expression\@107-108 IntLiteral\@107-108",
    'origins of case 8';
is walk("unit class U is Int;\nmethod m \{ 1 };\n"),
    "StatementList\@0-36 Statement::Expression\@0-36 Class\@0-36 Name\@11-12 Block\@13-36 Trait::Is\@13-19 Type::Simple\@16-19 Name\@16-19 Blockoid\@21-36 StatementList\@21-36 Statement::Expression\@21-35 Method\@21-35 Name\@28-29 Blockoid\@30-35 StatementList\@32-33 Statement::Expression\@32-33 IntLiteral\@32-33",
    'origins of case 9';

is Q[module MX is export { }].AST.statements[0].expression.traits[0].raku.subst(/\s+/, ' ', :g),
    'RakuAST::Trait::Is.new( name => RakuAST::Name.from-identifier("export") )',
    'a bare `is export` on a module has no argument';
