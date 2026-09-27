unit module ManualExportStashProtoAllMod;

# Same idiom as ManualExportStashProtoMod.rakumod, but declared in the
# `EXPORT::ALL` stash so it is reached only through `use ... :ALL`. See #9720.
my package EXPORT::ALL {
    our proto sub gamma($) {*}
    multi sub gamma(Int $x) { "gamma-int $x" }
    multi sub gamma(Str $x) { "gamma-str $x" }
}
