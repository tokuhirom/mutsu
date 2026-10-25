use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# A grouped declaration, `my ($a, @b) = RHS`, is one node the parser holds as
# written until its statement modifiers and loose tails have read it.

plan 13;

sub declaration-of(Str $source) { $source.AST.statements[0].expression }

is declaration-of('my ($a, $b) = 1, 2;').^name, 'RakuAST::VarDeclaration::Signature',
    'a grouped declaration is a signature declaration';
is declaration-of('my ($a, $b) = 1, 2 if True;').^name, 'RakuAST::VarDeclaration::Signature',
    'its statement modifier is the statement`s own';

is EVAL('my ($a, $b) = 1, 2; $a + $b'.AST), 3, 'a list assignment runs';
is EVAL('my ($a, $b) := (3, 4); $a * $b'.AST), 12, 'a list binding runs';
is EVAL('my ($a, $b) = 5, 6 if True; $a + $b'.AST), 11, 'a true if-modifier assigns';
is EVAL('my ($a, $b) = 5, 6 if False; $a.defined'.AST), False,
    'a false if-modifier still declares, without assigning';
is EVAL('my ($a, $b) = 5, 6 unless False; $b'.AST), 6, 'an unless-modifier assigns';
is EVAL('my ($a, $b) = 7, 8 with 1; $a'.AST), 7, 'a with-modifier assigns';
is EVAL('my ($a, $b) = 7, 8 without 1; $a.defined'.AST), False,
    'a without-modifier gates the initializer';
is EVAL('my $r = 0; $r += $_ for 1..3; $r'.AST), 6, 'a for-modifier on a plain statement';
is EVAL('my ($a, $b) = 4, 5 and $b'.AST), 5, 'a loose and-tail';
is EVAL('my ($a, @b) = 1, 2, 3; @b.elems'.AST), 2, 'a slurpy element';
is EVAL('my (\x, \y) = 3, 4; x + y'.AST), 7, 'sigilless elements are terms';
