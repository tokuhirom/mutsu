unit module UsesNativeCallThenLibs;
use NativeCall;
use NativeLibs;
our sub native-candidates() {
    &trait_mod:<is>.candidates.grep({ .signature ~~ :(Routine, :$native!) }).elems
}
