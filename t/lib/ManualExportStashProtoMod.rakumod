unit module ManualExportStashProtoMod;

# The "manual EXPORT stash" idiom (see ManualExportStashMod.rakumod / #7988)
# extended to a multi family: raku rejects `our multi sub` outright ("Cannot
# use 'our' with individual multi candidates. Please declare an our-scoped
# proto instead"), so an `our`-scoped proto is the only legal way to put a
# multi family into an export stash directly. See #9720.
my package EXPORT::DEFAULT {
    our proto sub delta($) {*}
    multi sub delta(Int $x) { "delta-int $x" }
    multi sub delta(Str $x) { "delta-str $x" }

    our sub eps() { "eps" }
}
