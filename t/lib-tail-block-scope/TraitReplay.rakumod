unit module TraitReplay;
my $applied = 0;
multi sub trait_mod:<is>(Routine:D $r, :$replay-traced!) is export {
    $applied++;
    $r.wrap(-> |c { "traced:" ~ callsame() });
}
sub applied is export { $applied }
sub tag($s) is export { "tag:$s" }
