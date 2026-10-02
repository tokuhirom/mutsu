use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# GH #10657: an indirect name `::(EXPR)` may be followed by static segments
# (`::($n)::Bar`) and/or a trailing `::` (`::($n)::`). mutsu stopped after the
# `)` and died with "Two terms in a row". Behaviour measured on Rakudo 2026.09,
# which resolves `::($n)::` to the package itself (not its stash).

plan 12;

package INT1 { our $x = 41 }
class INT2 { class Bar { class Baz { } }; our $v = 5 }
my $n = 'INT2';

is ::($n)::Bar.^name, 'INT2::Bar', 'a static segment after an indirect name';
is ::('INT2')::Bar::Baz.^name, 'INT2::Bar::Baz', 'several static segments';
is ::($n)::.^name, 'INT2', 'a trailing :: names the package itself';
is ::($n)::<$v>.raku, 'Any', 'a subscript after the trailing :: subscripts the package';
is ::($n)::('Bar').^name, 'INT2::Bar', 'a following dynamic segment still works';
my $our = 'OUR';
is $::($our)::INT1::x, 41, 'the sigiled form with a static tail is unaffected';
is ::($n)::Bar.new.^name, 'INT2::Bar', 'a method call on the looked-up type';

sub name-parts($src) {
    $src.AST.statements[*-1].expression.name.parts.map(*.^name).join(',')
}
is name-parts(Q|::("A")::Bar|),
    'RakuAST::Name::Part::Empty,RakuAST::Name::Part::Expression,RakuAST::Name::Part::Simple',
    'RakuAST keeps the static tail as Simple parts';
is name-parts(Q|::("A")::|),
    'RakuAST::Name::Part::Empty,RakuAST::Name::Part::Expression,RakuAST::Name::Part::Empty',
    'RakuAST ends a trailing :: with the Empty type object';
nok Q|::("A")::|.AST.statements[0].expression.name.parts[*-1].DEFINITE,
    'the trailing edge is the type object, not an instance';

is EVAL(Q|class INT3 { class Bar { } }; my $m = 'INT3'; ::($m)::Bar.^name|.AST), 'INT3::Bar',
    'the static tail round-trips through .AST.EVAL';
is EVAL(Q|class INT4 { }; ::('INT4')::.^name|.AST), 'INT4',
    'the trailing :: round-trips through .AST.EVAL';
