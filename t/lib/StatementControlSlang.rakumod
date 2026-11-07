# Fixture for t/lang/parsing/slang-statement-control-hook.t (ADR-15762): a slang
# whose grammar role adds a `statement-control:sym<twice>` production, with an
# action that attaches the RakuAST of a call to `run-twice`.
use v6.e.PREVIEW;

our role StatementControlGrammar {
    token statement-control:sym<twice> {
        'twice' <.end-keyword> <.ws> $<block>=<block>
    }
}

our role StatementControlActions {
    method statement-control:sym<twice>(Mu $/) {
        my $call = RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('run-twice'),
            args => RakuAST::ArgList.new($<block>.ast),
        );
        self.attach: $/, RakuAST::Statement::Expression.new(expression => $call);
    }
}

sub run-twice(&block) is export { block(); block() }

use Slangify StatementControlGrammar, StatementControlActions;
