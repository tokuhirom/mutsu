# Fixture for t/grammar/slang-actions-helper-methods.t (ADR-13220): an actions
# role with a `statement_control:sym<...>` candidate that no grammar declares.
# It belongs to a proto family Rakudo's grammar has, so slang activation treats
# it as a production handler (and rejects it by name) instead of an ignored
# helper.
use v6;

sub EXPORT is raw {
    my role UnknownCandidateActions {
        method statement_control:sym<frobnicate>(Mu $/) { callsame }
    }
    $ = $*LANG.define_slang(
        'MAIN',
        $*LANG.slang_grammar('MAIN'),
        $*LANG.actions.^mixin(UnknownCandidateActions)
    );
    Map.new
}
