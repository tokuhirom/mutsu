# A bare file (no `unit` declarator) that imports an operator and uses it in a
# load-time constant and in an exported sub.
use OpBare::Provider;

constant $BARE-CONST is export = 'a' ¢¿ 'b';

sub bare-op(Str $s) is export { 'c' ¢¿ $s }
