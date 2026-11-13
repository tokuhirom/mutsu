use Test;
use nqp;

# A `$` attribute `:=`-bound to an Array, a Hash or a plain value has no
# Scalar container: a raw read of it, and an `Attribute.set_value` that binds
# such a variable into an `@`/`%` slot, hand out the bound thing itself.
# AttrX::Mooish's lazy attributes rely on this ("attribute is de-mooified"
# checks `$attr.get_value($obj).VAR.^isa(Array)`). Every expectation here is
# rakudo's.

plan 12;

my @outer = 1;
my %outer = a => 1;
my $outer = 1;

class Holder {
    has Mu $!val;
    method bind-array()  { $!val := @outer }
    method bind-hash()   { $!val := %outer }
    method bind-value()  { $!val := 42 }
    method bind-scalar() { $!val := $outer }
    method assign(Mu $v is raw) { $!val = $v }
    method get() is raw { $!val }
}

my $h = Holder.new;
$h.bind-array;
is $h.get.VAR.^name, 'Array', 'raw read of an attribute bound to an Array';
$h.get.push(2);
is-deeply @outer, [1, 2], 'and it is the bound Array itself';

$h.bind-hash;
is $h.get.VAR.^name, 'Hash', 'raw read of an attribute bound to a Hash';

$h.bind-value;
is $h.get.VAR.^name, 'Int', 'raw read of an attribute bound to a value';

$h.bind-scalar;
is $h.get.VAR.^name, 'Scalar', 'an attribute bound to a scalar variable keeps its Scalar';

$h.assign([3]);
is $h.get.VAR.^name, 'Scalar', 'an assigned Array lives in a Scalar';

class Foo { has @.array; has %.hash }
my $arr-attr  = Foo.^attributes.first(*.name eq '@!array');
my $hash-attr = Foo.^attributes.first(*.name eq '%!hash');

sub set-raw(Mu $attr, Mu $inst is raw, Mu $v is raw) { $attr.set_value($inst, $v) }

my $inst = Foo.new;
set-raw($arr-attr, $inst, Array.new(1, 2));
is $arr-attr.get_value($inst).VAR.^name, 'Array',
  'set_value of a raw parameter bound to an Array binds the Array';

my Mu $bound := Array.new(3);
$arr-attr.set_value($inst, $bound);
is $arr-attr.get_value($inst).VAR.^name, 'Array',
  'set_value of a $-variable :=-bound to an Array binds the Array';

my $itemized = [4, 5];
$arr-attr.set_value($inst, $itemized);
is $arr-attr.get_value($inst).VAR.^name, 'Scalar',
  'set_value of a $-variable holding an Array binds its Scalar';

my Mu $bound-hash := Hash.new((b => 2));
$hash-attr.set_value($inst, $bound-hash);
is $hash-attr.get_value($inst).VAR.^name, 'Hash',
  'set_value of a $-variable :=-bound to a Hash binds the Hash';

class Prox is Proxy {
    has Mu $.val;
    method fill(Mu $inst is raw) is raw {
        $!val := Array.new;
        $!val.STORE((7, 8));
        set-raw($arr-attr, $inst, $!val);
        $!val
    }
}
my $p := Prox.new(FETCH => -> $ { 1 }, STORE => -> $, $ { });
my $inst2 = Foo.new;
$p.VAR.fill($inst2);
is $arr-attr.get_value($inst2).VAR.^name, 'Array',
  'a Proxy subclass\'s bound attribute is handed on without a Scalar';
is-deeply $arr-attr.get_value($inst2).List, (7, 8), 'with its contents';
