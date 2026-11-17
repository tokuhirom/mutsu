# A module whose CHECK phaser rewrites `$*CU`: every call of `cnt` gains an
# extra argument, so the rewritten module answers 2 where the source says 1.
unit module CheckRewriteModule;

sub cnt(*@a) is export { @a.elems }
sub f is export { cnt(7) }
sub g is export { cnt(1) + cnt(2) }
our sub qualified { cnt(3) }
sub inside { cnt(4) }
our $module-value = inside();

my $check-package;
sub check-package is export { $check-package }
sub package-name is export { $?PACKAGE.^name }

sub walk($n) {
    if $n.^name eq 'RakuAST::Call::Name' && $n.name.canonicalize eq 'cnt' {
        $n.args.push(RakuAST::IntLiteral.new(8));
    }
    if $n.^name eq 'RakuAST::Sub' && $n.name.canonicalize eq 'inside' {
        $check-package = $n.compile-time-value.package.^name;
    }
    $n.visit-children(&walk);
}
CHECK { walk($*CU) }
