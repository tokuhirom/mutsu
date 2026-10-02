use Test;
use nqp;

# Reading an attribute through introspection vivifies it, as in Rakudo:
# `.raku` / `.gist` / `dd` and `eqv` read every public attribute, and
# `Attribute.get_value` reads its own, so `nqp::attrinited` turns true for
# those. `.Str`, a private attribute, `eqv` against another type and `eqv`
# of an object with itself read nothing. mutsu#11003.

plan 14;

class A { has $.x; has @.l }

sub inited(\o, $name) { nqp::attrinited(o, A, $name) }

my $fresh := A.new;
nok inited($fresh, '$!x'), 'a fresh attribute is not inited';

my $r := A.new;
$r.raku;
ok inited($r, '$!x'), '.raku vivifies a scalar attribute';
ok inited($r, '@!l'), '.raku vivifies an array attribute';

my $g := A.new;
$g.gist;
ok inited($g, '$!x'), '.gist vivifies too';

my $v := A.new;
A.^attributes[0].get_value($v);
ok inited($v, '$!x'), 'Attribute.get_value vivifies its attribute';
nok inited($v, '@!l'), 'but not another one';

my $s := A.new;
$s.Str;
nok inited($s, '$!x'), '.Str reads no attribute';

my $d := A.new;
my $e := A.new;
my $eq = $d eqv $e;
ok inited($d, '$!x'), 'eqv vivifies the left operand';
ok inited($e, '@!l'), 'and the right one';

my $same := A.new;
my $eq2 = $same eqv $same;
nok inited($same, '$!x'), 'eqv of an object with itself reads nothing';

class B { has $.x }
my $other := A.new;
my $eq3 = $other eqv B.new;
nok inited($other, '$!x'), 'eqv against another type reads nothing';

class C { has $!p; has $.q }
my $c1 := C.new;
my $c2 := C.new;
my $eq4 = $c1 eqv $c2;
nok nqp::attrinited($c1, C, '$!p'), 'eqv leaves a private attribute alone';
ok nqp::attrinited($c1, C, '$!q'), 'and vivifies the public one';

my $m := C.new;
$m.raku;
nok nqp::attrinited($m, C, '$!p'), '.raku leaves a private attribute alone';
