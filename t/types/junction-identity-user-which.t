use Test;

plan 7;

class Id {
    has Str $.n;
    method new($n) { self.bless(:$n) }
    method WHICH(Id:D: --> ValueObjAt:D) { ValueObjAt.new("Id|$!n") }
}
my $a = Id.new("bar");
my $same = Id.new("bar");
my $other = Id.new("baz");

# `===` threads over a Junction; every eigenstate compares by its own user WHICH,
# also when no plain `===` ran first.
is ($a === any($same)).gist, 'any(True)', 'any: equal by WHICH';
is ($a === none($same)).gist, 'none(True)', 'none: equal by WHICH';
is ($a === one($same, $other)).gist, 'one(True, False)', 'one';
is ($a === all($same, $same)).gist, 'all(True, True)', 'all';
is ($other === none($same)).so, True, 'a different WHICH is none of them';
is ($a === none($same)).so, False, 'the same WHICH is not none of them';
my @kept = ($a => 1, $other => 2).grep(-> (:$key, :$value) { $key === none($same) }).map(*.value);
is-deeply @kept.List, (2,), 'a grep with a none() of value objects keeps the others';
