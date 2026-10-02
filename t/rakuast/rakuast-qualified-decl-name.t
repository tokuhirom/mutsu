use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# GH #10655: a `::`-qualified name is segmented into `Name::Part::Simple`
# parts wherever it appears — a declaration's own name, a type, a parent —
# and renders as `Name.from-identifier-parts("A","B")`, as Rakudo 2026.09
# does. Operator names that merely contain `::` stay one identifier.

plan 13;

sub decl-name($src) { $src.AST.statements[0].expression.name }

for (
    Q|class QD1::B { }|,   'class',
    Q|role QD2::B { }|,    'role',
    Q|grammar QD3::B { }|, 'grammar',
    Q|module QD4::B { }|,  'module',
    Q|package QD5::B { }|, 'package',
    Q|enum QD6::B <a b>|,  'enum',
    Q|subset QD7::B of Int|, 'subset',
) -> $src, $what {
    is decl-name($src).raku,
        qq|RakuAST::Name.from-identifier-parts("{$src.words[1].split('::')[0]}","B")|,
        "a qualified $what name is segmented";
}

is decl-name(Q|class QD8::B { }|).parts.map(*.name).join('|'), 'QD8|B',
    'the parts are walkable';
is Q|class QD9::B { }; QD9::B.new|.AST.statements[1].expression.operand.name.raku,
    'RakuAST::Name.from-identifier-parts("QD9","B")',
    'a qualified type used as a term is segmented too';
is decl-name(Q|class QD10 { }|).raku, 'RakuAST::Name.from-identifier("QD10")',
    'an unqualified name keeps from-identifier';

is EVAL(Q|class QE1::B { method m { 42 } }; QE1::B.new.m|.AST), 42,
    'a qualified class round-trips through EVAL';
is EVAL(Q|class QE2::B { }; class QE2K is QE2::B { }; QE2K.^mro.map(*.^name).join(',')|.AST),
    'QE2K,QE2::B,Any,Mu', 'a qualified parent round-trips through EVAL';
ok EVAL(Q|grammar QE3::G { token TOP { a } }; so QE3::G.parse("a")|.AST),
    'a qualified grammar round-trips through EVAL';
