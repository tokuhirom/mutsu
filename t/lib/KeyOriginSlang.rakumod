# Fixture for t/lang/parsing/slang-key-origin.t (ADR-15762, #13370, #16888): a
# `routine-declarator:sym<>` token that reads the core `sub` definition after a
# keyword of its own with `<routine-def=.key-origin('routine-def', 'sub')>`, as
# Slang::Mosdef does. The action attaches the node the core rule built.
use v6.e.PREVIEW;

our role KeyOriginGrammar {
    token routine-declarator:sym<fn> {
        'fn' <.end-keyword>
        <routine-def=.key-origin('routine-def', 'sub')>
    }
}

our role KeyOriginActions {
    method routine-declarator:sym<fn>(Mu $/) {
        self.attach: $/, RakuAST::Statement::Expression.new(expression => $<routine-def>.ast);
    }
}

use Slangify KeyOriginGrammar, KeyOriginActions;
