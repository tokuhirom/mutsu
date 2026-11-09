# Fixture for t/lang/parsing/slang-statement-control-passthrough.t (#13370): actions-only
# overrides of four core statement-control productions that answer the core node.
use v6.e.PREVIEW;
our role PassStatementControlGrammar { }
our role PassStatementControlActions {
    method statement-control:sym<given>(Mu $/) { callsame }
    method statement-control:sym<for>(Mu $/) { callsame }
    method statement-control:sym<if>(Mu $/) { callsame }
    method statement-control:sym<while>(Mu $/) { callsame }
}
use Slangify PassStatementControlGrammar, PassStatementControlActions;
