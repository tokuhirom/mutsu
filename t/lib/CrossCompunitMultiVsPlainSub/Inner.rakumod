unit module Inner;

# Only these `multi` candidates exist in THIS compunit; neither is exported.
# `Outer.rakumod` declares an unrelated, unexported-vs-exported PLAIN sub of
# the identical bare name -- nothing below may ever resolve to it.
multi sub to-toml(Str:D $s --> Str:D) { qq{"$s"} }
multi sub to-toml(Int:D $i --> Str:D) { ~$i }

sub render(%h --> Str:D) is export {
    my Str:D @out;
    for %h.sort(*.key) -> $pair {
        @out.push: "{$pair.key}={to-toml($pair.value)}";
    }
    @out.join(',');
}

sub render-list(@l --> Str:D) is export {
    @l.map({ .&to-toml }).join(',');
}
