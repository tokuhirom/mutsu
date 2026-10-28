# `OUR.WHO.BIND-KEY: .key, .value for Other::` in `package EXPORT::MANDATORY`
# copies a module's whole stash: its types and variables as well as its
# routines (annotations' `ANN`, #15259).
my class Marker {
    method ^parameterize(Mu, Mu $target is raw) is raw { "marker for " ~ $target.^name }
}
module ExportMandatoryTypes::Stash {
    OUR::<Marker> := Marker;
    our only infix:<tag>(Mu $a, Mu $b) is looser<...> { "tagged" }
    our $level = 5;
}
my package EXPORT {
    package MANDATORY {
        OUR.WHO.BIND-KEY: .key, .value for ExportMandatoryTypes::Stash::;
    }
}
