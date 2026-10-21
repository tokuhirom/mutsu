use Test;

plan 7;

# `$!attr := Proxy.new(...)` seats the Proxy as the attribute: reads FETCH and
# writes STORE, inside the method and through the `is rw` accessor (#14266).
my @stored;
class C {
    has $.bar is rw;
    method bind {
        $!bar := Proxy.new(
            FETCH => -> $ { 44 },
            STORE => -> $, $v { @stored.push: $v });
    }
    method inside { $!bar }
}
my $c = C.new;
$c.bind;
is $c.inside, 44, 'a method reads the bound Proxy through FETCH';
is $c.bar, 44, 'the accessor reads the bound Proxy through FETCH';
is $c.bar + 1, 45, 'numeric context FETCHes';
$c.bar = 3;
is-deeply @stored, [3], 'assigning through the accessor calls STORE';
@stored = ();
$c.bar = 'x';
is-deeply @stored, ['x'], 'a second assignment calls STORE again';
is $c.bar, 44, 'the Proxy is still the attribute';
is $c.inside.WHAT.^name, 'Int', 'FETCH result is the value';
