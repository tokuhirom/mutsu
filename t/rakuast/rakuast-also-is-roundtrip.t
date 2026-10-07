use v6;
use Test;
use MONKEY-SEE-NO-EVAL;

# `also is Parent;` and `class ::Name` keep their source spelling in `.AST`
# (#12200) and the AST still evaluates to the same classes.

plan 5;

my $ast = Q|class RtBase { method who { 'base' } }
class RtKid { also is RtBase; method kid { 1 } }|.AST;
like $ast.gist, /'Statement::Also'/, '`also is Parent;` is a Statement::Also';

EVAL $ast;
is ::('RtKid').new.who, 'base', 'the lowered class inherits from the parent';
is-deeply ::('RtKid').^parents(:local).map(*.^name).List, ('RtBase',), 'the parent is recorded once';

my $colons = Q|class ::RtColons { method m { 42 } }|.AST;
like $colons.gist, /'Name::Part::Empty.new'/, '`class ::Name` keeps the leading empty part';
EVAL $colons;
is ::('RtColons').m, 42, 'and the lowered class is declared';
