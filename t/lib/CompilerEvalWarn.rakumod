use nqp;
unit class CompilerEvalWarn;

has Mu $.compiler is built(:bind) = "Raku";
has @!warnings;
method TWEAK(--> Nil) {
    $!compiler := nqp::getcomp(nqp::decont($!compiler)) if nqp::istype($!compiler, Str);
}
method eval(str $code) {
    CONTROL { when CX::Warn { @!warnings.push($_); .resume } }
    $!compiler.eval($code, :interactive(1))
}
method warnings() { @!warnings.List }
