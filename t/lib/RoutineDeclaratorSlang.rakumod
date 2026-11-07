# Fixture for t/routines/slang-routine-declarator-hook.t (ADR-15762, #13370): a
# slang whose grammar role adds a `routine-declarator:sym<fun>` production whose
# action attaches a RakuAST::Sub.
use v6.e.PREVIEW;

our role FunGrammar {
    token routine-declarator:sym<fun> {
        'fun' <.end-keyword> <.ws> $<name>=[<[\w-]>+] <.ws> $<block>=<block>
    }
}

our role FunActions {
    method routine-declarator:sym<fun>(Mu $/) {
        my $sub = RakuAST::Sub.new(
            name => RakuAST::Name.from-identifier(~$<name>),
            body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                    expression => RakuAST::Call::Name.new(
                        name => RakuAST::Name.from-identifier('run-it'),
                        args => RakuAST::ArgList.new($<block>.ast),
                    )))),
        );
        self.attach: $/, RakuAST::Statement::Expression.new(expression => $sub);
    }
}

sub run-it(&block) is export { block() }

use Slangify FunGrammar, FunActions;
