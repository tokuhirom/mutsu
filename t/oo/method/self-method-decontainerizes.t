use Test;

# `.self` hands back the invocant's VALUE, not its container, so it is not a
# member of the raw-invocant family (`.item`, `.snitch`) that ADR-0067 surveys.
# Every expectation below was measured against rakudo 2026.07.

plan 22;

# --- the headline: a Scalar container is never identical to the value it holds
my $a = 42;
nok $a.self =:= $a, 'an Int in a Scalar: .self is not the container';
ok  $a.item =:= $a, '.item, by contrast, hands the container back';
ok  $a.self =:= $a.self, 'two decontainerized reads of the same Int are identical';
ok  $a.self =:= 42, 'a decontainerized Int is identical to the literal';
nok $a =:= $a.self, 'and the same with the operands the other way round';

my $s = 'str';
nok $s.self =:= $s, 'a Str in a Scalar';

class C {}
my $c = C.new;
nok $c.self =:= $c, 'an instance in a Scalar';

my $undef;
nok $undef.self =:= $undef, 'an uninitialized Scalar';

my $held = [1, 2];
nok $held.self =:= $held, 'an Array held in a Scalar is still behind a container';
nok $held.self.self =:= $held, 'chained .self decontainerizes just the same';

my $list = (1, 2);
nok $list.self =:= $list, 'a List in a Scalar';

# --- an aggregate IS its own container, so `.self` hands back the same thing
my @arr = 1, 2;
ok @arr.self =:= @arr, '@-sigil: .self is the Array itself';
ok @arr.self =:= @arr.self, 'twice over';
my @bound := @arr;
ok @arr.self =:= @bound, 'and through a bound alias of it';

my %h = a => 1;
ok %h.self =:= %h, '%-sigil: .self is the Hash itself';

# --- a `:=` binding owns no Scalar, so `.self` really is the same thing
my $int-bind := 42;
ok $int-bind.self =:= $int-bind, ':=-bound to a literal';
my $arr-bind := @arr;
ok $arr-bind.self =:= $arr-bind, ':=-bound to an Array';
my $hash-bind := %h;
ok $hash-bind.self =:= $hash-bind, ':=-bound to a Hash';
my $obj-bind := C.new;
ok $obj-bind.self =:= $obj-bind, ':=-bound to a fresh object';

# ... but binding to another SCALAR aliases that scalar's container.
my $alias := $a;
nok $a.self =:= $alias, ':=-bound to a $ scalar still aliases its container';

# --- a readonly binding that DOES own a container reports one (rakudo: Scalar)
sub takes-ro($v) { nok $v.self =:= $v, 'a non-rw parameter owns a container' }
takes-ro(42);
sub takes-rw($v is rw) { nok $v.self =:= $v, 'an `is rw` parameter owns one too' }
my $target = 9;
takes-rw($target);
