# Fixture for t/lang/parsing/slang-for-otherwise.t (#13370): a slang whose grammar
# role re-writes the core `statement-control:sym<for>` token with a trailing
# `otherwise` block and whose actions role `callsame`s into the core action and
# hands the block to `replace-otherwise` -- the shape of Slang::Otherwise.
use v6.e.PREVIEW;

# A HLL lookup in a NQP Match object.
my sub lookup(Mu \match, str $key) {
    use nqp;
    nqp::atkey(
      nqp::findmethod(match,'hash')(match),
      $key
    ).?ast
}

our role ForOtherwiseGrammar {
    rule statement-control:sym<for> {
        <.block-for><.kok> {}
        <.vetPerlForSyntax>
        :my $*GOAL := '{';
        :my $*BORG := {};
        <EXPR>
        <pointy-block>
        [ otherwise $<otherwise>=<.pointy-block> ]?
    }
}

our role ForOtherwiseActions {
    method statement-control:sym<for>(Mu $/) {
        if lookup($/, 'otherwise') -> $otherwise {
            callsame.replace-otherwise($otherwise);
        }
    }
}

use Slangify ForOtherwiseGrammar, ForOtherwiseActions;
