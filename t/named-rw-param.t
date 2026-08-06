use v6;
use Test;

# A named `is rw` scalar parameter aliases the caller's variable through a
# shared ContainerRef cell, like the positional arm
# (todo/tickets/named-rw-param-writeback-is-lost.md). Raku requires such a
# param to be non-optional (`:$x!`). Expected values verified against raku.

plan 7;

sub set7(:$x! is rw) { $x = 7 }
my $a = 1;
set7(x => $a);
is $a, 7, "fatarrow named rw argument writes back";

my $b = 1;
set7(:x($b));
is $b, 7, "colonpair named rw argument writes back";

sub bump(Int :$n! is rw) { $n = $n + 1 }
my Int $t = 5;
bump(n => $t);
is $t, 6, "typed named rw argument writes back";

throws-like { set7(x => 1) }, Exception,
    message => /'writable'/,
    "a literal named argument for an rw param dies";

sub rawread(:$x! is raw) { $x }
is rawread(x => 41), 41, "is raw named param accepts a literal";

# The alias is live during the call: a nested rw sub sees the same cell.
sub outer(:$v! is rw) { inner($v) }
sub inner($w is rw) { $w = 99 }
my $c = 0;
outer(v => $c);
is $c, 99, "named rw relayed to a positional rw param writes the caller";

# is copy named param does not alias.
sub copies(:$x! is copy) { $x = 3 }
my $k = 1;
copies(x => $k);
is $k, 1, "named is copy still copies";

done-testing;
