# Fixture for t/grammar/slang-actions-helper-methods.t (ADR-13220): an actions
# role overriding `blockoid`, a production of Rakudo's grammar that mutsu's
# slang support does not recognize. Slang activation must reject it by name
# rather than mistake it for a helper method.
use v6;

sub EXPORT is raw {
    my role UnknownProductionActions {
        method blockoid(Mu $/) { callsame }
    }
    $ = $*LANG.define_slang(
        'MAIN',
        $*LANG.slang_grammar('MAIN'),
        $*LANG.actions.^mixin(UnknownProductionActions)
    );
    Map.new
}
