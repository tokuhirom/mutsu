use nqp;

# Fixture for t/vm/codegen/adr0116-trir-op-bodies.t: the op-body shapes
# ADR-0116 D2.4 made cheaper, each called more than once (a TRIR cache bug
# gives a right first answer and a wrong later one).

# A free variable that is a `package P { my ... }` lexical -- JSON::Fast's
# `$ws` shape. TRIR now caches the binding it reads from the package's
# lexical store instead of resolving it by name on every call, so a later
# assignment has to be seen by the next call. (An assignment writes through
# the entry's cell; replacing the entry bumps the cache's generation. A
# `:=` re-bind from inside a sub, the other way to replace it, dies on the
# untyped path too, #9238.)
package P {
    my $tbl = nqp::list_i(10, 20, 30);
    our sub probe(int $i) { nqp::atpos_i(nqp::decont($tbl), $i) }
    our sub reassign() { $tbl = nqp::list_i(70, 80, 90); Nil }
}

say 'probe => ', P::probe(0), ' ', P::probe(2);
say 'probe => ', P::probe(1);
P::reassign();
say 'reassigned => ', P::probe(0), ' ', P::probe(2);
say 'reassigned => ', P::probe(1);

# `Uni:D` is met by every normalization form; a form is met only by itself.
sub first-code(Uni:D \codes) { nqp::atpos_i(codes, 0) }
sub nfd-only(NFD:D \codes) { nqp::elems(codes) }
say 'uni => ', first-code("abc".NFC), ' ', first-code("é".NFD), ' ',
    first-code(Uni.new(0x41, 0x42)), ' ', first-code("x".NFKC);
say 'nfd => ', nfd-only("é".NFD), ' ', (try nfd-only("é".NFC)) // 'rejected';

# `nqp::findcclass` / `findnotcclass` on an ASCII string (one byte per
# grapheme) and on one that is not, including windows that end early and
# start past the end.
sub scan(str $s, int $from, int $count) {
    nqp::findnotcclass(nqp::const::CCLASS_WORD, $s, $from, $count) ~ '/'
      ~ nqp::findcclass(nqp::const::CCLASS_WHITESPACE, $s, $from, $count)
}
for 1..2 {
    say 'scan => ', scan('abc_12 x-y', 0, 10), ' ', scan('abc_12 x-y', 2, 3), ' ',
        scan('abc_12 x-y', 7, 99), ' ', scan('abc', 5, 2), ' ',
        scan("ab\r\ncd e", 0, 9), ' ', scan("é\x[301]b c", 1, 9);
}
