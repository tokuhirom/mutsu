use v6;
use Test;

# From DB::Xoos (DB::Xoos::SQL's gen-quote): a raw `\v` bound to the topic `$_`
# of a for/map over plain values aliases no Scalar, so `v =:= v."name"()` holds.

plan 6;

sub dyn(\v)   { v =:= try v."{v.^name}"() }
sub stat(\v)  { v =:= v."Str"() }
sub slf(\v)   { v =:= v.self }

my %h = a => 'abc';

is-deeply %h.keys.map({ dyn($_) }).list, (True,), 'dynamic method name on map topic';
for %h.keys { ok dyn($_), 'dynamic method name on for topic' }
for <a> { ok stat($_), 'static method name on for topic' }
for <a> { ok slf($_),  '.self on for topic' }
ok dyn("lit"), 'literal argument';

my $v = 'x';
nok dyn($v), 'a real Scalar variable is still a container';
