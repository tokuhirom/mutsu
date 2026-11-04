use Test;

# Found via deredere (Gumbo::Binding): `our $k is export := cglobal(LIB, ...)`
# binds NativeCall's lazy Proxy. mutsu FETCHed it at the declaration and STOREd
# the value back, so a missing native library died while the module loaded;
# rakudo only fetches when the variable is read.

plan 6;

my $log = '';
sub lazy() is rw {
    Proxy.new(
        FETCH => -> $ { $log ~= 'F'; 42 },
        STORE => -> $, $v { $log ~= "S($v)" },
    )
}

our $o := lazy();
is $log, '', 'our $o := proxy does not fetch or store at the declaration';

our $p is export := lazy();
is $log, '', 'the same with is export';

my $m := lazy();
is $log, '', 'my $m := proxy (already lazy)';

is $o, 42, 'reading the bound variable fetches';
nok $log.contains('S'), 'reading never stores';

$log = '';
$o = 7;
ok $log.contains('S(7)'), 'assigning to the bound variable stores through the proxy';
