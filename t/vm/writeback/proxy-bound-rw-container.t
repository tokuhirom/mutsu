use Test;

plan 8;

# An `is rw` routine whose tail names a variable bound to a Proxy hands back
# the Proxy itself, so assigning to the call runs its STORE.
my $s = 1;
my @stores;
my $p := Proxy.new(FETCH => -> $ { $s }, STORE => -> $, $v { @stores.push($v); $s = $v });

sub outer-var is rw { $p }
outer-var() = 7;
is-deeply @stores, [7], 'sub tail naming a captured Proxy-bound variable';

sub local-var is rw { my $q := $p; $q }
local-var() = 8;
is-deeply @stores, [7, 8], 'sub tail naming a local bound to a Proxy';

# A `$!attr` bound to a Proxy.
class Holder {
    has $.a is rw;
    my @log;
    submethod TWEAK {
        my $store = 1;
        $!a := Proxy.new(FETCH => -> $ { $store }, STORE => -> $, $v { @log.push($v); $store = $v });
    }
    method via-attr is rw { $!a }
    method via-self is rw { self.a }
    method log { @log }
}
Holder.^add_method('nick', Holder.^method_table<a>);

my $h = Holder.new;
$h.a = 3;
is-deeply $h.log, [3], 'public rw accessor stores through the Proxy';
$h.via-attr = 4;
is-deeply $h.log, [3, 4], 'rw method returning $!attr stores through the Proxy';
$h.via-self = 5;
is-deeply $h.log, [3, 4, 5], 'rw method returning self.attr stores through the Proxy';
$h.nick = 6;
is-deeply $h.log, [3, 4, 5, 6], 'an alias of the accessor stores through the Proxy';
is $h.a, 6, 'the Proxy FETCHes the stored value';

# A plain (non-Proxy) variable still gets its own cell.
my $plain = 1;
sub plain-var is rw { $plain }
plain-var() = 9;
is $plain, 9, 'rw sub tail naming an ordinary variable still writes it';
