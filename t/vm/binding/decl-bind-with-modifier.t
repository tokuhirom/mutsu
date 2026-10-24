use Test;

# A `:=` declaration under a statement modifier declares the name
# unconditionally and binds only when the modifier runs. The bound expression
# was dropped: `my $x := EXPR with $t` left `$x` unbound.

plan 6;

sub w($v) { my $name := ($_ ~ "!") with $v; $name }
is w("abc"), 'abc!', 'with: binds the topicalized expression';

sub wo($v) { my $name := "none" without $v; $name }
is wo(Str), 'none', 'without: binds when undefined';

sub i($v) { my $name := 5 if $v; $name }
is i(1), 5, 'if: binds when true';
nok i(0).defined, 'if: unbound when false';

my @a = 1, 2;
my $r := @a[0] with 1;
$r = 9;
is @a[0], 9, 'the bind aliases the element';

use nqp;
sub n($v) { my $name := (nqp::istype($_, Str) ?? $_ !! "no") with $v; $name }
is n("s"), 's', 'an nqp test in the bound expression';
