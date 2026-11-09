# Fixture for t/lang/parsing/slang-variable-shim.t (#13370): a statement-control
# token that reads a variable with the core `<variable>` rule (Qwiratry's
# `givenroot`), and a trait call that passes an argument to the core `<trait>`.
use v6.e.PREVIEW;

our role VariableShimGrammar {
    token statement-control:sym<bindroot> {
        'bindroot' <.end-keyword> <.ws> $<root>=<variable> <.ws> $<block>=<block>
    }
}

our role VariableShimActions {
    method statement-control:sym<bindroot>(Mu $/) {
        self.attach: $/, RakuAST::Statement::Expression.new(
            expression => RakuAST::Call::Name.new(
                name => RakuAST::Name.from-identifier('bind-root'),
                args => RakuAST::ArgList.new($<root>.ast, $<block>.ast),
            ),
        );
    }
}

sub bind-root($root, &block) is export { block($root) }

use Slangify VariableShimGrammar, VariableShimActions;
