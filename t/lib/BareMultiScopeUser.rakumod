# Receives a code value for a multi family it never imported (#11004).
unit module BareMultiScopeUser;
sub bmsu-apply(&f, $arg) is export { f($arg) }
sub bmsu-apply-in-block(&f, $arg) is export { (-> $x { &f($x) })($arg) }
