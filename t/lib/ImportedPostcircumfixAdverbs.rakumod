class PCM does Associative is export {
    has %.h = a => 1, b => 2;
    method AT-KEY(\k) { %!h{k} }
}
multi sub postcircumfix:<{ }>(PCM:D $c, \key, *%adv) is export {
    "user key={key.raku} adv={%adv.raku}"
}
