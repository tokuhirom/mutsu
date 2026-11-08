# Fixture for t/lang/parsing/slang-postfix-expr-override.t (ADR-15762 slice 3,
# #13369): an actions role overriding the core `POSTFIX-EXPR` action. The postfix
# `++` is rewritten into a call of `bump`.
use v6.e.PREVIEW;

our role PostfixRewriteGrammar { }

our role PostfixRewriteActions {
    method POSTFIX-EXPR(Mu $/) {
        my $core = callsame;
        my $ast = $/.ast;
        return $core unless $ast.defined && $ast ~~ RakuAST::ApplyPostfix;
        return $core unless $ast.postfix.operator eq '++';
        my $rewritten = RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('bump'),
            args => RakuAST::ArgList.new($ast.operand),
        );
        self.attach: $/, $rewritten;
        $rewritten
    }
}

sub bump($v) is export { $v + 100 }

use Slangify PostfixRewriteGrammar, PostfixRewriteActions;
