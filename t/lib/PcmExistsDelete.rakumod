unit module PcmExistsDelete;

class PCM does Associative is export {
    has %.h = a => 1;
    method AT-KEY(\k) { %!h{k} }
}


multi sub postcircumfix:<{ }>(PCM:D $c, \key, *%adv) is export {
    "import key={key.raku} adv={%adv.raku}"
}

