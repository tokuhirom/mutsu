use Test;

plan 9;

# An object destructured by an all-named sub-signature is unpacked through its
# Capture: the named parts are its public attributes, and one the sub-signature
# does not name is an unexpected named argument unless `|` / `*%` takes it.
class Ge { has $.left; has $.right; has $!priv = 1 }
my $p = Ge.new(left => 'a', right => 1);

sub only-left(Ge (:$left)) { "m" }
sub with-slip(Ge (:$left, |)) { "m" }
sub both(Ge (:$left, :$right)) { "m" }
sub rest(Ge (:$left, *%rest)) { "m" ~ %rest.keys }
sub kept(Ge $o (:$left, :$right)) { $left ~ $right }

nok (try { only-left($p) }).defined, 'a surplus public attribute is refused';
is-deeply (try { with-slip($p) }), "m", '`|` takes it';
is-deeply (try { both($p) }), "m", 'naming every public attribute matches';
is-deeply (try { rest($p) }), "mright", '`*%rest` takes it';
is kept($p), 'a1', 'values bind from the accessors';

multi pick(Ge (:$left)) { "one" }
multi pick(Ge (:$left, :$right)) { "two" }
is pick($p), 'two', 'multi dispatch skips the candidate that leaves an attribute over';

class Pt { has $.x; has $.y }
sub only-x(Pt (:$x)) { "m" }
nok (try { only-x(Pt.new(x => 1, y => 2)) }).defined, 'the same for another class';
nok (try { only-x(Pt.new(x => 1)) }).defined, 'an unset public attribute still counts';
sub pair-ok(Pair (:$key, :$value)) { $key ~ $value }
is pair-ok(Pair.new('a', 'b')), 'ab', 'a Pair is unaffected';
