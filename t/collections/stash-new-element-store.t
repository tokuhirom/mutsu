use Test;

plan 8;

# `Stash.new` is a Hash-backed instance: element stores keep its class
# instead of turning it into a plain Hash (#16346).

my $s = Stash.new;
is $s.^name, "Stash", "Stash.new is a Stash";
$s<x> = 1;
is $s.^name, "Stash", "an element assignment keeps the class";
is $s<x>, 1, "the stored value reads back";

my $t = Stash.new;
$t{'z'} = 3;
is $t.^name, "Stash", "curly-brace subscript store keeps the class";

my $u = Stash.new;
$u<k>++;
is $u.^name, "Stash", "an element increment keeps the class";
is $u<k>, 1, "the incremented value reads back";

my $v = $s;
is $v.^name, "Stash", "a copy of the stored Stash is still a Stash";

class H2 is Stash { }
my $h = H2.new;
$h<a> = 1;
is $h.^name, "H2", "a user subclass keeps its class";
