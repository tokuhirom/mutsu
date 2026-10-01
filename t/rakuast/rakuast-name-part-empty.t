use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# GH #10646: the empty edge of a `RakuAST::Name` — the leading `::` of
# `::Foo` / `::($x)` and the trailing `::` of a stash lookup `Foo::`.
# Rakudo 2026.09 spells it `RakuAST::Name::Part::Empty`; rakudo/rakudo#6771
# renames it to `RakuAST::Name::Part::EmptyEdge`, so both spellings are
# accepted while `.AST` keeps emitting `Empty`. Expected shapes measured on
# Rakudo 2026.09.

plan 31;

my \S = RakuAST::Name::Part::Simple;
my \E = RakuAST::Name::Part::Empty;
my \EE = RakuAST::Name::Part::EmptyEdge;

# --- the part classes ------------------------------------------------------

is E.^name, 'RakuAST::Name::Part::Empty', 'Empty is a registered type object';
is E.new.raku, 'RakuAST::Name::Part::Empty.new', 'an Empty instance renders without parens';
ok E.new.DEFINITE, 'Empty.new is an instance';
nok E.DEFINITE, 'Empty itself is the type object';
ok E ~~ RakuAST::Name::Part, 'the Empty type object is a Name::Part';
ok E.new ~~ RakuAST::Name::Part, 'an Empty instance is a Name::Part';
is E.^mro.map(*.^name).join(','), 'RakuAST::Name::Part::Empty,RakuAST::Name::Part,Any,Mu',
    'Empty has the Name::Part MRO';
nok E.new ~~ RakuAST::Node, 'a name part is not a RakuAST::Node';
nok S.new("x") ~~ RakuAST::Node, 'nor is a Simple part';
nok S.new("x") ~~ RakuAST::Name, 'a Simple part is not a Name despite its namespace';
is EE.new.raku, 'RakuAST::Name::Part::EmptyEdge.new', 'the EmptyEdge spelling is constructible';

# --- RakuAST::Name.new rendering ------------------------------------------

is RakuAST::Name.new(S.new("Foo")).raku, 'RakuAST::Name.from-identifier("Foo")',
    'a one-identifier Name renders as from-identifier';
is RakuAST::Name.new(S.new("Foo"), S.new("Bar")).raku,
    'RakuAST::Name.from-identifier-parts("Foo","Bar")',
    'an all-identifier Name renders as from-identifier-parts';
is RakuAST::Name.new().raku, 'RakuAST::Name.new()', 'an empty Name';
is RakuAST::Name.new(S.new("Foo"), E).raku, q:to/END/.chomp, 'a trailing empty edge is the type object';
RakuAST::Name.new(
  RakuAST::Name::Part::Simple.new("Foo"),
  RakuAST::Name::Part::Empty
)
END
is RakuAST::Name.new(S.new("Foo"), E).parts.map({ .DEFINITE }).join(','), 'True,False',
    '.parts returns the type object unchanged';
is RakuAST::Name.new(E.new, S.new("A"), S.new("B")).raku, q:to/END/.chomp, 'a leading empty edge is an instance';
RakuAST::Name.new(
  RakuAST::Name::Part::Empty.new,
  RakuAST::Name::Part::Simple.new("A"),
  RakuAST::Name::Part::Simple.new("B")
)
END

# --- read direction ---------------------------------------------------------

is Q|Foo::|.AST.raku, q:to/END/.chomp, 'an unresolved stash lookup is an argument-less Call::Name';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::Call::Name.new(
      name => RakuAST::Name.new(
        RakuAST::Name::Part::Simple.new("Foo"),
        RakuAST::Name::Part::Empty
      )
    )
  )
)
END
is Q|MY::|.AST.statements[0].expression.^name, 'RakuAST::Term::Name',
    'a pseudo-package stash resolves at parse time';
is Q|class NPE1 { }; NPE1::|.AST.statements[1].expression.^name, 'RakuAST::Term::Name',
    'a declared package resolves at parse time';
is Q|class NPE2::Inner { }; NPE2::|.AST.statements[1].expression.^name, 'RakuAST::Term::Name',
    'the stub package of a qualified declaration resolves too';
is Q|class NPE5::Inner { }; NPE5|.AST.statements[1].expression.^name, 'RakuAST::Type::Simple',
    'a bareword naming that stub package is a type';
is Q|::|.AST.statements[0].expression.name.raku, q:to/END/.chomp, 'a bare :: is both edges';
RakuAST::Name.new(
  RakuAST::Name::Part::Empty.new,
  RakuAST::Name::Part::Empty
)
END
is Q|::("Int")|.AST.statements[0].expression.name.parts.map(*.^name).join(','),
    'RakuAST::Name::Part::Empty,RakuAST::Name::Part::Expression',
    'an indirect lookup has a leading empty edge';

# --- write direction (EVAL) -------------------------------------------------

is EVAL(RakuAST::Term::Name.new(RakuAST::Name.new(S.new("Int"), E))).^name, 'Stash',
    'Term::Name of a stash name lowers to the stash';
is EVAL(RakuAST::Call::Name.new(name => RakuAST::Name.new(S.new("Int"), E))).^name, 'Stash',
    'an argument-less Call::Name of a stash name lowers to the stash';
is EVAL(RakuAST::Term::Name.new(RakuAST::Name.new(E.new, S.new("Int")))).^name, 'Int',
    'a leading empty edge before an identifier is the plain name';
is EVAL(RakuAST::Type::Simple.new(RakuAST::Name.new(E.new, S.new("Int")))).^name, 'Int',
    'Type::Simple accepts a leading empty edge';
is EVAL(RakuAST::Term::Name.new(RakuAST::Name.new(S.new("Int"), EE))).^name, 'Stash',
    'the EmptyEdge spelling lowers the same way';
is EVAL(Q|class NPE3 { our $v = 7 }; NPE3::<$v>|.AST), 7,
    'a stash lookup round-trips through .AST.EVAL';
is EVAL(Q|class NPE4 { our $v = 8 }; my $s = NPE4::; $s<$v>|.AST), 8,
    'a stash bound to a variable round-trips through .AST.EVAL';
