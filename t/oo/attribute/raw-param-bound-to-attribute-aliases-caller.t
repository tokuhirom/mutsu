use Test;
plan 5;

class Slot {
    has $!value is default(Nil);
    method bind(Mu $topic is raw) { $!value := $topic }
    method set($v) { $!value = $v }
    method get { $!value }
}

# The very first call of the program, from a top-level lexical.
my $w = 1;
my $sl = Slot.new;
$sl.bind($w);
$sl.set(7);
is $w, 7, 'a write through the attribute reaches the caller variable';
$w = 11;
is $sl.get, 11, 'the attribute sees later writes to the caller variable';

my $w2 = 1;
my $s2 = Slot.new;
$s2.bind($w2);
$s2.set(8);
is $w2, 8, 'a second bind behaves the same';

sub inner { my $x = 1; my $s = Slot.new; $s.bind($x); $s.set(3); $x }
is inner(), 3, 'a sub-local variable aliases too';

my $lit = Slot.new;
lives-ok { $lit.bind(5); $lit.set(6) }, 'binding a literal stays harmless';
