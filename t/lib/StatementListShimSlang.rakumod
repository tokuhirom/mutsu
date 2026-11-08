# Fixture for t/lang/parsing/slang-statementlist-shim.t (ADR-16888): a statement-control
# production whose token calls the core `<statementlist>` rule and whose action
# wraps the statements it got in a call.
use v6.e.PREVIEW;

our role GroupGrammar {
    token statement-control:sym<group> {
        'group' <.end-keyword> <.ws> '{' $<body>=<statementlist> '}'
    }
}

our role GroupActions {
    method statement-control:sym<group>(Mu $/) {
        my $block = RakuAST::Block.new(
            body => RakuAST::Blockoid.new($<body>.ast),
        );
        my $call = RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('run-group'),
            args => RakuAST::ArgList.new($block),
        );
        self.attach: $/, RakuAST::Statement::Expression.new(expression => $call);
    }
}

sub run-group(&body) is export { body() }

use Slangify GroupGrammar, GroupActions;
