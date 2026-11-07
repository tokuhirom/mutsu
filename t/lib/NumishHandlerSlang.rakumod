# Fixture for t/lang/parsing/slang-use-sub-arg.t (#17055): `sub EXPORT` receives
# the `&sub` the importing unit passes to `use`, and the actions role calls it on
# every Int literal, as overload::constant does with its handlers.
use v6.e.PREVIEW;

my $handler;

our role Numish {
    method numish(Mu $/) {
        my $core := callsame;
        my $ast = $/.ast;
        return $core unless $handler.defined && $ast.defined && $ast ~~ RakuAST::IntLiteral;
        my $new = RakuAST::StrLiteral.new($handler($ast.value));
        self.attach: $/, $new;
        $new
    }
}

sub EXPORT(*@h) {
    $handler = @h[0];
    $*LANG.refine_slang('MAIN', role { }, Numish);
    BEGIN Map.new
}
