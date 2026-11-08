# Fixture for t/grammar/slang-own-helper-rule.t (ADR-16888): a grammar role that
# declares a rule rakudo's grammar does not have (EEL's `eel-body` is one, called
# only from a grammar method), next to a rule of its own that nothing refers to.
use v6.e.PREVIEW;

our role OwnHelperGrammar {
    token zzz-helper-nobody-calls { 'q' }
    rule zzz-helper-rule { 'a' 'b' }
}

sub EXPORT() {
    $*LANG.refine_slang('MAIN', OwnHelperGrammar, role { });
    BEGIN Map.new
}
