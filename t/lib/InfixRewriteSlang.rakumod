# Fixture for t/lang/parsing/slang-core-action-override.t (ADR-15762 slice 3, #13369):
# an actions role overriding the core `INFIX-EXPR` action, calling `callsame` to
# get the core node and re-attaching a rewritten one (`+` becomes `-`).
use v6.e.PREVIEW;

our role InfixRewriteGrammar { }

our role InfixRewriteActions {
    method INFIX-EXPR(Mu $/) {
        my $core = callsame;
        my $ast = $/.ast;
        return $core unless $ast.defined && $ast ~~ RakuAST::ApplyInfix;
        return $core unless $ast.infix.operator eq '+';
        my $rewritten = RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('infix:<->'),
            args => RakuAST::ArgList.new($ast.left, $ast.right),
        );
        self.attach: $/, $rewritten;
        $rewritten
    }

    # `PREFIX-EXPR` likewise: the prefix `!` is dropped (`!$x` is `$x`).
    method PREFIX-EXPR(Mu $/) {
        my $core = callsame;
        my $ast = $/.ast;
        return $core unless $ast.defined && $ast ~~ RakuAST::ApplyPrefix;
        return $core unless $ast.prefix.operator eq '!';
        self.attach: $/, $ast.operand;
        $ast.operand
    }
}

use Slangify InfixRewriteGrammar, InfixRewriteActions;
