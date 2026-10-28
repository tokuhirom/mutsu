use Test;

plan 7;

# A role whose `is rw` AT-KEY / AT-POS serves the element container (a Proxy)
# and that also has an ASSIGN-POS: a key subscript `<k>` / `{"k"}` is an
# associative subscript, so it is served by AT-KEY and never reaches
# ASSIGN-POS, whatever the receiver is -- a variable, a mixin, a method's
# result or a sub's result. Red's ColumnMethods (`.num1<ble> = 13`) is this
# shape; the key reached ASSIGN-POS and died numifying it.

my @log;
role Meths {
    multi method AT-KEY(\SELF where { .returns ~~ Int }: $key where { $_ ~~ Str }) is rw {
        Proxy.new(FETCH => -> $ { 0 }, STORE => -> $, $v { @log.push: "key $key=$v" })
    }
    method ASSIGN-POS(\SELF where { .returns ~~ Int }: $index, $value) {
        @log.push: "ASSIGN-POS $index";
        SELF.AT-POS($index) = $value
    }
    method AT-POS(\SELF where { .returns ~~ Int }: $key) is rw {
        Proxy.new(FETCH => -> $ { 0 }, STORE => -> $, $v { @log.push: "pos $key=$v" })
    }
}
class Col does Meths { has $.returns = Int }
class Model { method num1 { Col.new } }

my $m = Model.new;
$m.num1<ble> = 13;
is @log.tail, "key ble=13", 'a method result receiver';
for $m { .num1<k> = 1 }
is @log.tail, "key k=1", 'a topic method result';
my $blk = { .num1<q> = 2 };
$blk($m);
is @log.tail, "key q=2", 'inside a block';
my $e = Col.new;
$e<y> = 4;
is @log.tail, "key y=4", 'a variable receiver';
$e[2] = 5;
is @log.tail, "pos 2=5", 'a positional subscript still reaches ASSIGN-POS, which assigns through AT-POS';

role OnlyKey {
    method AT-KEY($key) is rw { Proxy.new(FETCH => -> $ { 0 }, STORE => -> $, $v { @log.push: "mixin $key=$v" }) }
}
my $c = Col.new but OnlyKey;
$c<ble> = 13;
is @log.tail, "mixin ble=13", 'a mixed-in role AT-KEY on a variable';
class Plain { }
my $d = Plain.new but OnlyKey;
$d<z> = 6;
is @log.tail, "mixin z=6", '... on a plain class';
