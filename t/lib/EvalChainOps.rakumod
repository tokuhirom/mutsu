unit module EvalChainOps;
sub evc-sub() is export { 'evc-sub' }
sub postfix:<!>($n) is export { "fact($n)" }
sub infix:<+++>($a, $b) is export { "pp($a,$b)" }
