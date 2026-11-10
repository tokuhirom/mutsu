# Fixture for t/lang/parsing/slang-host-sees-unit-class-in-body.t (#13370): a
# `routine-declarator:sym<mk>` whose action compiles its block to a Code object
# with `meta-object` while the unit is still being parsed, as Qwiratry's `mold` does.
use v6.e.PREVIEW;

our role MkGrammar {
    token routine-declarator:sym<mk> {
        'mk' <.end-keyword> <.ws> $<block>=<block>
    }
}

our role MkActions {
    method routine-declarator:sym<mk>(Mu $/) {
        my $sub = RakuAST::Sub.new(signature => RakuAST::Signature.new, body => $<block>.ast.body);
        my $code = $sub.meta-object;
        self.attach: $/, $sub;
    }
}

use Slangify MkGrammar, MkActions;
