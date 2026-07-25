unit module DoForValueSub;

sub plainsub(Str $s) is export { "P:$s" }

sub namedsub(Str $s, Bool :$upper) is export {
    $upper ?? "N:" ~ $s.uc !! "N:$s"
}
