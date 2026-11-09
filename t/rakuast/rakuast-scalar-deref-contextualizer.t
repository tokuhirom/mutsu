use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 8;

# `@$x` and `%$h` are Contextualizer::List / Contextualizer::Hash over the bare
# variable (not a parenthesized one, not a written `.hash` call).
my $decl = 'my ($x, $h); ';
sub expr(Str $code) { ($decl ~ $code).AST.statements[1].expression }

is expr('@$x.push(4)').operand.^name, 'RakuAST::Contextualizer::List', '@$x is a List contextualizer';
is expr('@$x.push(4)').operand.target.^name, 'RakuAST::Var::Lexical', 'over the bare variable';
is expr('%$h.elems').operand.^name, 'RakuAST::Contextualizer::Hash', '%$h is a Hash contextualizer';
is expr('%$h.elems').operand.target.^name, 'RakuAST::Var::Lexical', 'over the bare variable';
is ($decl ~ '@$x.push(4)').AST.DEPARSE.lines.tail.trim, '@$x.push(4)', '.DEPARSE writes @$x back';
is ($decl ~ '%$h.elems').AST.DEPARSE.lines.tail.trim, '%$h.elems', '.DEPARSE writes %$h back';

# The write direction keeps both working.
my $x = [1, 2, 3];
is EVAL(Q[@$x.elems].AST), 3, 'EVAL of a tree with @$x';
my $h = {a => 1, b => 2};
is EVAL(Q[%$h.elems].AST), 2, 'EVAL of a tree with %$h';
