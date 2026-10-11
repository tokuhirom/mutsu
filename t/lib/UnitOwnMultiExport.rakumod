multi sub columns() is export { 42 }
multi sub columns(Str $s) is export { "s:$s" }
proto sub matches(|) is export {*}
multi sub matches() { "m0" }
multi sub matches(Int $i) { "m$i" }
my sub EXPORT(*@names) {
    Map.new: @names.map: {
        my ($in, $out) = .split(q[:], 2);
        $out ?? (Pair.new: "&$out", UNIT::{"&$in"}) !! (UNIT::{"&$_"}:p)
    }
}
