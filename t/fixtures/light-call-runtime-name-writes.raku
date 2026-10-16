# Fixture for t/vm/frames/light-call-carries-runtime-name-writes.t: writes
# whose target name exists only at run time, made two light calls down.
my $backing = 41;
sub bind-up($name) {
    CALLER::CALLER::.BIND-KEY($name, Proxy.new(
        FETCH => -> $ { $backing },
        STORE => -> $, $value { $backing = $value },
    ));
}
sub install($name) { bind-up($name) }
my $a = 1;
install('$a');
say "bound => $a";
$a = 9;
say "stored => $backing";
our $g = 1;
sub set-g($n) { $::($n) = 5 }
sub via($n) { set-g($n) }
via('g');
say "named => $g";
