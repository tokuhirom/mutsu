use Test;

# An assignment evaluates to its target's container, so passing one as an
# argument binds an `is rw` parameter to the assigned variable, for a method
# call as for a sub call (#13934, Intl::CLDR's `Currency.new(blob, ($offset
# -= 2))`). Checked against rakudo.

plan 7;

class K { method inc($x is rw) { $x += 10 } }
sub inc($x is rw) { $x += 10 }

sub via-method() { my $o = 10; K.inc(($o -= 2)); $o }
is via-method(), 18, 'a compound assignment passed to a method';

sub via-sub() { my $o = 10; inc(($o -= 2)); $o }
is via-sub(), 18, '...and to a sub';

sub plain-assign() { my $o = 1; K.inc($o = 5); $o }
is plain-assign(), 15, 'a plain assignment passed to a method';

my $t = 10; K.inc(($t -= 2));
is $t, 18, 'at the top level';

sub rw-param($offset is rw) { K.inc(($offset -= 2)); $offset }
my $a = 10;
is rw-param($a), 18, 'the assigned variable is an rw parameter';
is $a, 18, '...whose caller sees the write';

class Cur { method new(\blob, uint64 $offset is rw) { $offset += 3; self.bless } }
sub native(\blob, uint64 $offset is rw) { Cur.new(blob, ($offset -= 2)); $offset }
my uint64 $q = 10;
is native('x', $q), 11, 'a native rw parameter, as Intl::CLDR passes it';
