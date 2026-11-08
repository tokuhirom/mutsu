# Fixture for t/routines/slang-routine-declarator-term.t (ADR-15762, #13370): a
# `routine-declarator:sym<lam>` token used in term position; its action attaches
# an anonymous RakuAST::Sub.
use v6.e.PREVIEW;

our role LamGrammar {
    token routine-declarator:sym<lam> {
        'lam' <.end-keyword> <.ws> $<block>=<block>
    }
}

our role LamActions {
    method routine-declarator:sym<lam>(Mu $/) {
        self.attach: $/, RakuAST::Sub.new(body => $<block>.ast.body);
    }
}

use Slangify LamGrammar, LamActions;
