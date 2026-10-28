use v6;
use Test;

# AccessorFacade (#15127): a trait handler composes a role with a CALL-ME onto
# the Method object; calling the method then runs that CALL-ME, invocant first.

plan 6;

my role Provider[&get, &set] {
    method CALL-ME(*@args) is rw {
        my $self = @args[0];
        Proxy.new(FETCH => sub ($) { &get($self) }, STORE => sub ($, $v) { &set($self, $v) });
    }
}
multi trait_mod:<is>(Method $r, :@accessor-facade! (&getter, &setter)) {
    $r does Provider[&getter, &setter];
}
class Bar {
    has $.boot is rw = 'foo';
    sub get_bar(Bar $s) { $s.boot }
    sub set_bar(Bar $s, $x) { $s.boot = $x }
    method boom() is rw is accessor-facade(&get_bar, &set_bar) { }
    method plain() { 'plain' }
}
my $a = Bar.new;
is $a.boom, 'foo', 'a read goes through CALL-ME';
$a.boom = 'yada';
is $a.boot, 'yada', 'an assignment goes through CALL-ME';
is $a.boom, 'yada', 'and reads back';
my $b = Bar.new;
is $b.boom, 'foo', 'another instance has its own value';
is $a.plain, 'plain', 'a method without the trait is untouched';
is Bar.^find_method('boom').does(Provider), True, 'the Method object does the role';
