use Test;
plan 6;

my class Unending does Iterator {
    has $!n = 0;
    method pull-one() { $!n++ }
    method is-lazy(--> True) { }
}
sub mk() { Seq.new: Unending.new }

is mk().lazy.^name, 'Seq', '.lazy on an unpulled iterator Seq returns without draining it';
ok mk().lazy.is-lazy, 'and the result is lazy';
is mk().lazy.head(3).List, (0, 1, 2), 'and still yields its elements';
ok so mk(), 'the first element decides Bool for an unbounded iterator Seq';
ok so mk().lazy, '... also after .lazy';
nok so Seq.new(class :: does Iterator { method pull-one() { IterationEnd } }.new), 'an empty one is false';
