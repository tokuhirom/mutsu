use Test;

plan 5;

# `infix:<=>` decontainerizes its key. A Proxy subclass whose FETCH is raw
# returns another Proxy; the key is the value at the end of that chain.
my class Binder is Proxy {
    has $!value;
    method new is raw { callwith :&FETCH, :&STORE }
    my method FETCH is raw { $!value }
    my method STORE(Mu $topic is raw --> Nil) { $!value := $topic }
}
my $inner := Proxy.new(FETCH => -> $ { IntStr.new(2, "k1") }, STORE => -> $, $ { });
my $b := Binder.new;
$b = $inner;

is ($b => 1).key.VAR.^name, 'IntStr', 'plain => FETCHes a Proxy chain key';

my @buf;
@buf.BIND-POS(0, $b);
is (@buf Z=> (7,)).map(*.key.^name).join, 'IntStr', 'Z=> FETCHes a Proxy chain key';
is Map.new(@buf Z=> (7,)).raku, 'Map.new((:k1(7)))', 'Map.new over Z=> pairs';
is Map.new(reverse @buf.head(1) Z=> (7,)).raku, 'Map.new((:k1(7)))',
    'reverse over a Seq of Proxy elements';
sub h(:@of is raw = @buf.head(1)) { Map.new: reverse @of Z=> (7,) }
is h().raku, 'Map.new((:k1(7)))', 'through an `is raw` @-parameter default';
