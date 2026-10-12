use Test;

plan 6;

# A public attribute declared `is built(False)` cannot be set by `.new`, so
# Rakudo's default `.raku` leaves it out and the `eqv` built on that `.raku`
# ignores it (IRC::Log::Colabti's entries differ only in such a `$.log`).
class C { has $.a; has $.l is built(:bind); has $.b is built(False) }
my $x = C.new(a => 1, l => 1);
is $x.raku, 'C.new(a => 1, l => 1)', '.raku omits a public is built(False) attribute';

class Owner { has $.id; has $.cache is built(False) }
my $o1 = Owner.new(id => 1);
my $o2 = Owner.new(id => 1);
$o1.cache = 1 if False;
ok $o1 eqv $o2, 'eqv ignores a public is built(False) attribute';

class Log { has @.entries is built(False); method add($e) { @!entries.push($e) } }
class Entry { has $.text is built(:bind); has $.log is built(False) }
my $l1 = Log.new; my $l2 = Log.new;
$l2.add(1);
ok Entry.new(text => "hi", log => $l1) eqv Entry.new(text => "hi", log => $l2),
    'entries pointing at different logs are eqv';
ok Entry.new(text => "hi", log => $l1) !eqv Entry.new(text => "yo", log => $l1),
    'a buildable attribute still decides eqv';

class D { has $.a; has $.l is built(:bind); }
ok D.new(a => 1, l => 1) !eqv D.new(a => 1, l => 2), 'is built(:bind) attributes are compared';
is D.new(a => 1, l => 1).raku, 'D.new(a => 1, l => 1)', '.raku keeps is built(:bind) attributes';
