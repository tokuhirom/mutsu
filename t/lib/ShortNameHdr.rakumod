unit class ShortNameHdr;

my grammar Header {
    token TOP { <[a..z]>+ }
}

method parse(Str $s) {
    Header.parse($s) ?? "grammar-ok" !! "grammar-fail"
}
