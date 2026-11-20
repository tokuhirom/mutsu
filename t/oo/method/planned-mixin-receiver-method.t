use Test;

# #18267 stage 4: a method call on a runtime mixin type over a built-in
# `Attribute` (`$attr does R`) takes the plain-method lane and replays its
# call plan, including calls whose `is raw` arguments are shared cells
# relayed from a caller's container or an attribute. Every call repeats, so
# the replayed calls are the ones checked.

plan 13;

role Tagged {
    has $.hits = 0;
    method bump(Mu $target is raw, Mu $value is raw) is raw {
        $!hits++;
        $target = $value;
        $target
    }
    method peek(Mu $target is raw) { $target }
}

class Holder {
    has $!slot;
    has $!val;
    method setup(Mu \cell) { $!slot := cell; $!val := my $ = 0; self }
    method relay($attr, $v) { $!val = $v; $attr.bump($!slot, $!val) }
    method slot-is(Mu \cell) { $!slot =:= cell }
}

class Box { has $.n is rw = 0 }

my $attr = Box.^attributes[0];
$attr does Tagged;
is $attr.^name, 'Attribute+{Tagged}', 'the receiver is a mixin type over Attribute';

my $x = 1;
$attr.bump($x, $_ * 10) for ^5;
is $x, 40, 'an is raw parameter writes through to the caller variable on every call';
is $attr.hits, 5, 'the role attribute counts every call';

my $cell = 7;
my $h = Holder.new.setup($cell);
$h.relay($attr, $_ + 100) for ^4;
is $cell, 103, 'a cell relayed from an attribute is aliased, not copied';
ok $h.slot-is($cell), 'the attribute still holds the caller cell';
is $attr.hits, 9, 'relayed calls reach the role method';

my $o = Box.new;
is-deeply (^3).map({ $attr.peek($o).^name }).List, <Box Box Box>, 'an instance argument';
is-deeply (^3).map({ $attr.peek(42) }).List, (42, 42, 42), 'a literal argument';
is-deeply (^3).map(-> $v { my $y = $v; $attr.peek($y) }).List, (0, 1, 2),
    'a fresh variable argument each call';

my $errors = 0;
for ^3 { try { $attr.bump(5, 1); CATCH { default { $errors++ } } } }
is $errors, 3, 'writing through an is raw parameter bound to a literal still dies';

# The same method, alternately with a plain value and with a cell, at the same
# argument type: neither shape's plan may answer for the other.
my @log;
for ^4 -> $i {
    my $plain = $i;
    my $c := my $ = $i;
    $attr.bump($plain, $i + 50);
    $attr.bump($c, $i + 60);
    @log.push: "$plain/$c";
}
is @log.join(','), '50/60,51/61,52/62,53/63', 'plain and cell arguments interleave';

# A mixin over a user class keeps working as before.
role Twice { method twice(Mu $v is raw) { $v * 2 } }
my $b = Box.new but Twice;
is-deeply (^3).map({ $b.twice($_) }).List, (0, 2, 4), 'a mixin over a user class';
is $attr.hits, 20, 'every call ran the role method';
