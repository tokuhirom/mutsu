use Test;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

# #16787: a `my` variable keeps its declarator docs in the parsed tree, so the
# declaration prints a `.declarator-docs(...)` call and `.WHY` answers a
# Doc::Declarator. mutsu recorded no docs for a variable.

plan 12;

sub decl($src) { $src.AST.statements[0].expression }

my $trailing = decl('my $x; #= after');
is $trailing.WHY.trailing.raku, '("after",)', 'a trailing comment documents the variable';
is $trailing.WHY.leading.raku, '()', 'and it has no leading text';
isa-ok $trailing.WHY, RakuAST::Doc::Declarator, 'WHY is a Doc::Declarator';
is $trailing.WHY.WHEREFORE.^name, 'RakuAST::VarDeclaration::Simple', 'whose WHEREFORE is the declaration';

my $leading = decl("#| plain\nmy \$y;");
is $leading.WHY.leading.raku, '("plain",)', 'a leading comment documents the variable';

my $init = decl("#| p\nmy \$z = 1;");
is $init.WHY.leading.raku, '("p",)', 'a variable with an initializer';

is $trailing.raku.lines.tail(5).join('|'), ').declarator-docs(|  trailing => (|    "after",|  )|)',
    'the declaration prints its docs';

nok decl('my $plain;').WHY.defined, 'an undocumented variable has no WHY';

# the variable takes its comment, so the next declaration does not
my $two = Q[my $a; #= first
sub f {}].AST;
is $two.statements[0].expression.WHY.trailing.raku, '("first",)', 'the variable has the comment';
nok $two.statements[1].expression.WHY.defined, 'the sub after it has none';

# the docs of a variable do not reach the runtime table or $=pod
is EVAL(Q[my $v = 5; #= doc
$v]), 5, 'a documented variable still runs';
is EVAL(Q[#| docs
sub k {}
my $v; #= var docs
&k.WHY.Str]), 'docs', 'and a sub documented next to it keeps its docs';
