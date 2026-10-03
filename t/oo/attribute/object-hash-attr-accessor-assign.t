use Test;

plan 4;

# Assigning pairs to an object-hash attribute through its rw accessor keeps
# the key objects (it used to stringify a Hash key to "x\t1").
class MH is rw { has %.hash{Any} }
my %k = x => 1;

my $v = MH.new;
$v.hash{$%k} = [3];
$v.hash .= grep: { True };
is-deeply $v.hash.keys[0].hash, {x => 1}, '.= grep keeps a Hash key';

my $w = MH.new;
$w.hash{$%k} = [3];
$w.hash = $w.hash.grep: { True };
is-deeply $w.hash.keys[0].hash, {x => 1}, 'assigning the grep result keeps it';
is $w.hash{$%k}, [3], 'and the entry is still found by that key';

my $n = MH.new;
$n.hash = (1 => 'a', 2 => 'b');
is $n.hash.keys.sort.join(','), '1,2', 'Str-keyed pairs still assign';
