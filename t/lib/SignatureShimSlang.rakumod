# Fixture for t/routines/signature/slang-signature-shim.t (ADR-15762, #13370): a
# `routine-declarator:sym<fun>` token that calls the core `<signature>` rule and
# builds a RakuAST::Sub from the signature and block it matched.
use v6.e.PREVIEW;

our role FunGrammar {
    token routine-declarator:sym<fun> {
        'fun' <.end-keyword> <.ws> $<name>=[<[\w-]>+] <.ws>
        '(' <.ws> $<signature>=<signature> <.ws> ')' <.ws> $<block>=<block>
    }
}

our role FunActions {
    method routine-declarator:sym<fun>(Mu $/) {
        my $sub = RakuAST::Sub.new(
            name      => RakuAST::Name.from-identifier(~$<name>),
            signature => $<signature>.ast,
            body      => $<block>.ast.body,
        );
        self.attach: $/, RakuAST::Statement::Expression.new(expression => $sub);
    }
}

use Slangify FunGrammar, FunActions;
