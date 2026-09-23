# Fixture for t/vm/codegen/adr0112-trir-caller-frames.t: `install` is a TRIR
# routine whose untyped callee reaches two routine frames up. Called twice, so
# both the resolving call and a resolution-cache hit run the chunk.
my $backing = 41;
sub bind-two-frames-up($name) {
    CALLER::CALLER::.BIND-KEY($name, Proxy.new(
        FETCH => -> $ { $backing },
        STORE => -> $, $value { $backing = $value },
    ));
}
sub install($name) { bind-two-frames-up($name) }
my $a = 1;
my $b = 2;
install('$a');
install('$b');
say "bound => $a $b";
$b = 73;
say "stored => $backing";

