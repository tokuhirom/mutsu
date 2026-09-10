# Deliberately NO `unit` declarator, and a class whose name is the module's own
# name: that is what puts `End::Unit::Qualified` into `package_declaring_units`
# and so under the #7797 qualified-name gate. A class named anything else would
# not be registered for this module at all, and the gate would stay permissive.
class EndUnitQualified {
    has $.n = 5;
    method twice() { $!n * 2 }
}

sub end-unit-marker() is export { 'loaded' }

END {
    # Runs at program exit, long after this module's load finished -- and, in
    # the case #7836 is about, after the EVAL unit that loaded it is gone.
    say 'END saw ', EndUnitQualified.new.twice;
}
