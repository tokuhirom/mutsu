use nqp;

# A module file WITHOUT `unit`: its file-scope lexicals are the compunit's own.
# This is the shape of Rakudo's `Telemetry.rakumod` (#11797).
my $snaps := nqp::create(IterationBuffer);
my $plain = [1, 2, 3];

sub rebind-add() is export { nqp::push($snaps, 1) }

sub rebind-take() is export {
    my $new := $snaps;
    $snaps := nqp::create(IterationBuffer);
    nqp::elems($new)
}

sub rebind-take-plain() is export {
    my $old := $plain;
    $plain := [9];
    $old.elems
}

sub rebind-current-plain() is export { $plain.elems }
