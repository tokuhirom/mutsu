# Fixture for t/lang/parsing/slang-parametric-lexical-role.t: a module whose `sub EXPORT`
# refines the main slang with an instance of a LEXICAL PARAMETRIC `my role`
# (`Numish[%opts]`), as overload::constant does with its handlers.
use v6.e.PREVIEW;
my role Numish[%opts] {
    method numish(Mu $/) {
        my $core := callsame;
        my $ast = $/.ast;
        return $core unless $ast.defined && $ast ~~ RakuAST::IntLiteral;
        my $new = RakuAST::IntLiteral.new($ast.value * %opts<factor>);
        self.attach: $/, $new;
        $new
    }
}
sub EXPORT(*@a) {
    my %opts = factor => @a[0];
    $*LANG.refine_slang('MAIN', role {}, Numish[%opts]);
    BEGIN Map.new
}
