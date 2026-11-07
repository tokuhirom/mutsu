# Fixture for t/lang/parsing/slang-numish-anonymous-actions.t (#17008): the actions role is an
# anonymous `role { ... }` that declares the `numish` override itself.
use v6.e.PREVIEW;
sub EXPORT() {
    $*LANG.refine_slang('MAIN', role { }, role {
        method numish(Mu $/) {
            my $core := callsame;
            my $ast = $/.ast;
            return $core unless $ast.defined && $ast ~~ RakuAST::IntLiteral;
            my $new = RakuAST::IntLiteral.new($ast.value * 10);
            self.attach: $/, $new;
            $new
        }
    });
    BEGIN Map.new
}
