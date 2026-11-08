# Test helper for t/modules/compunit/compunit-check-phasers-attr.t: its
# `sub EXPORT` pushes closures onto the importing unit's `$!check-phasers`
# list, as Qwiratry's `BodyRewrite.install` does.
use nqp;
sub EXPORT() {
    my @log;
    my $phasers := nqp::getattr($*CU, RakuAST::CompUnit, '$!check-phasers');
    nqp::push($phasers, { @log.push("pushed check phaser") });
    Map.new('@PUSHED-LOG' => @log)
}
