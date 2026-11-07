# Fixture for t/lang/parsing/slang-numish-lexical-actions.t (#17008): a module whose `sub EXPORT` refines
# the main slang with a LEXICAL `my role` actions role that overrides
# the core `numish` action, as overload::constant does.
use v6.e.PREVIEW;

my role Numish {
    method numish(Mu $/) {
        my $core := callsame;
        my $ast = $/.ast;
        return $core unless $ast.defined && $ast ~~ RakuAST::IntLiteral;
        my $new = RakuAST::IntLiteral.new($ast.value * 10);
        self.attach: $/, $new;
        $new
    }
}

sub EXPORT() {
    $*LANG.refine_slang('MAIN', role { }, Numish);
    BEGIN Map.new
}
