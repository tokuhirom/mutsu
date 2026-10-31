use Test;

# The Proxy `substr-rw` / `subbuf-rw` hand back outside an assignment aliases
# the source variable's container, in the sub form, the method form and inside
# a routine, and both of its closures are shared compiled bodies (#14572).
# Checked against rakudo.
plan 11;

my $s = "abcdef";
my $r := substr-rw($s, 1, 2);
is $r, 'bc', 'the sub form reads the window';
$r = "XYZ";
is $s, 'aXYZdef', 'a store splices into the source';
$r = "Q";
is $s, 'aQdef', 'the window follows the stored length';

my $t = "hello world";
my $m := $t.substr-rw(0, 5);
$m = "HOWDY";
is $t, 'HOWDY world', 'the method form splices too';

sub in-routine {
    my $u = "hello";
    my $p := substr-rw($u, 0, 1);
    $p = "J";
    $u
}
is in-routine(), 'Jello', 'inside a routine';

my $w = "abc";
my $q := substr-rw($w, 1, 1);
$w = "xyz";
is $q, 'y', 'a write to the source shows through the Proxy';

my $b = Buf.new(1, 2, 3, 4, 5);
my $br := subbuf-rw($b, 1, 2);
is-deeply $br, Buf.new(2, 3), 'subbuf-rw reads the window';
$br = Buf.new(9, 9, 9);
is-deeply $b, Buf.new(1, 9, 9, 9, 4, 5), 'and splices into the buffer';
my $b2 = Buf.new(1, 2, 3);
my $br2 := $b2.subbuf-rw(0, 1);
$br2 = Buf.new(7);
is-deeply $b2, Buf.new(7, 2, 3), 'the method form of subbuf-rw';

my $alias = $s;
$r = "Z";
is $s, 'aZdef', 'the source variable is updated';
is $alias, 'aQdef', 'a copy taken earlier is not';
