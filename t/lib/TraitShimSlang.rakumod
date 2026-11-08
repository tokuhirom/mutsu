# Fixture for t/oo/trait/slang-trait-shim.t (ADR-15762, #13370): a
# `routine-declarator:sym<fun>` token that calls the core `<trait>` rule and
# builds a RakuAST::Sub carrying the trait it matched.
use v6.e.PREVIEW;

our role FunGrammar {
    token routine-declarator:sym<fun> {
        'fun' <.end-keyword> <.ws> $<name>=[<[\w-]>+] <.ws>
        $<trait>=<trait> <.ws> $<block>=<block>
    }
}

our role FunActions {
    method routine-declarator:sym<fun>(Mu $/) {
        my $sub = RakuAST::Sub.new(
            name      => RakuAST::Name.from-identifier(~$<name>),
            traits    => [$<trait>.ast],
            body      => $<block>.ast.body,
        );
        self.attach: $/, RakuAST::Statement::Expression.new(expression => $sub);
    }
}


use Slangify FunGrammar, FunActions;
