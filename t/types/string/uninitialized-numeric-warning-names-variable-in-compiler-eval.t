use Test;
use lib $*PROGRAM.parent(3).add('lib');
use CompilerEvalWarn;

# From CodeUnit (t/01-basic.rakutest): the "Use of uninitialized value $x"
# warning of a numeric op names its variable even when the unit was compiled by
# the compiler object's .eval from a module's method (the op's name record must
# be keyed by the index of the op itself, past any lexical-import marker
# emitted before it).
plan 2;

my $e = CompilerEvalWarn.new;
$e.eval(q/my $x; my $y = $x + 1/);
is $e.warnings.map(*.Str.lines.head).join('|'),
  'Use of uninitialized value $x of type Any in numeric context', 'the warning names $x';

my $g = CompilerEvalWarn.new;
$g.eval(q/my $a = 3; $a + 1/);
is $g.warnings.elems, 0, 'no warning for a defined operand';
