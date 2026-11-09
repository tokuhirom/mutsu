# Fixture for t/lang/parsing/slang-statement-control-action.t (#13370): a slang whose
# actions role overrides the core `statement-control:sym<given>` action and
# `callsame`s into it, as Qwiratry::Query::Slang::Topic does.
use v6.e.PREVIEW;

our role GivenOverrideGrammar { }

our role GivenOverrideActions {
    method statement-control:sym<given>(Mu $/) {
        my $core = callsame;
        my $ast = $/.ast;
        return $core unless $ast.defined && $ast ~~ RakuAST::Statement::Given;
        my $rewritten = RakuAST::Statement::Expression.new(
            expression => RakuAST::Call::Name.new(
                name => RakuAST::Name.from-identifier('traced-given'),
                args => RakuAST::ArgList.new($ast.source, $ast.body),
            ),
        );
        self.attach: $/, $rewritten;
        $rewritten
    }
}

sub traced-given($topic, &body) is export {
    @*TRACE.push("given {$topic.raku}") if @*TRACE.defined;
    body($topic)
}

use Slangify GivenOverrideGrammar, GivenOverrideActions;
