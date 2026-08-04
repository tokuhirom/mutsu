unit module SamewithGatherModule;

# The shape `Digest::SHA3`'s `Keccak` uses: a `proto` whose wide candidate ends
# in `gather for samewith <the narrow candidate's arguments> { ... }`, called
# through an exported entry point. `Keccak` itself is NOT exported, so the
# gather body's `samewith` has to resolve a name that is only visible inside
# this module — while the body runs after `hashit` has returned.
our proto hashit($) is export {*}
multi hashit(Str $s) { samewith $s.encode }
multi hashit(Blob $b) {
    [~] Keccak $b, delimitedSuffix => 0x06, outputByteLen => 2,
                   rate => 1088, capacity => 512
}

our proto Keccak(
    Blob $inputBytes,
    byte :$delimitedSuffix,
    UInt :$outputByteLen is copy,
    UInt :$rate where * %% 8,
    UInt :$capacity where $rate + $capacity == 1600,
) {*}

multi Keccak($inputBytes, :$delimitedSuffix, :$rate, :$capacity) {
    gather loop { take "b($delimitedSuffix,$rate,$capacity)" }
}

multi Keccak($inputBytes, :$delimitedSuffix, :$outputByteLen is copy, :$rate, :$capacity) {
    gather for samewith $inputBytes, :$delimitedSuffix, :$rate, :$capacity {
        take "w:$_";
        $outputByteLen -= 1;
        last if $outputByteLen <= 0;
    }
}
