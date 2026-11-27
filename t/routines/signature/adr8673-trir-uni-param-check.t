use Test;
use nqp;

# A `Uni:D` / `NFD:D` parameter of a TRIR routine (JSON::Fast's
# `unjsonify-string(Uni:D \codes, int $pos)`) is checked from the type tables
# alone when nothing can rebind the name (#8673): no type capture, no package
# alias, no subset of it. The answers must be the general check's: a `Uni`
# of any form satisfies `Uni`, a form satisfies only its own name, and a
# `Str` or a type object is refused. Every expected value is rakudo 2026.07's.

plan 9;

sub take-uni(Uni:D \codes, int $n) {
    my $out := nqp::create(Uni);
    nqp::while(nqp::elems(codes), nqp::push_i($out, nqp::shift_i(codes)));
    nqp::strfromcodes($out) ~ $n
}
sub take-nfd(NFD:D \codes) { nqp::elems(codes) }
sub take-nfc(NFC:D \codes) { nqp::elems(codes) }
sub scan(str $s) {
    take-uni(nqp::strtocodes($s, nqp::const::NORMALIZE_NFD, nqp::create(NFD)), 1)
}

is scan('a/b'), 'a/b1', 'an NFD binds to Uni:D through a TRIR call';
is scan("\x[e9]-x"), "\x[e9]-x1", 'a non-ASCII one too';
is scan(''), '1', 'and an empty one';
is take-nfd("\x[e9]".NFD), 2, 'an NFD binds to NFD:D';
throws-like { take-nfc("\x[e9]".NFD) }, X::TypeCheck::Binding,
  'an NFD does not bind to NFC:D';
throws-like { take-uni('abc', 2) }, X::TypeCheck::Binding,
  'a Str does not bind to Uni:D';
throws-like { take-uni(Uni, 2) }, Exception, 'nor does the Uni type object';

# Once a type capture has been bound anywhere, the name could be rebound, so
# the check takes the general path; the answers stay the same.
sub capture(::T $x) { T.^name }
is capture(42), 'Int', 'a type capture binds';
is scan('c:d'), 'c:d1', 'Uni:D still binds after a capture was seen';
