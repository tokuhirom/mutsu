class SuppMod::Thing {
    my grammar Header {
        token TOP { \w+ }
    }
    method check(Str $s) { so Header.parse($s) }
}
