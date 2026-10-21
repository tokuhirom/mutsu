use Test;
use nqp;

plan 5;

# The container `Attribute.auto_viv_container` hands out for a `$` attribute
# carries the attribute's type, and `nqp::clone_nd` of it keeps it: a store into
# it type-checks like `$obj.attr = $value` (#14266, AttrX::Mooish stores the
# builder's value with `$!val = $value`).
class C {
    has Str $.s is rw;
    has $.any;
}
my Mu $s := C.^attributes[0].auto_viv_container;
throws-like { $s = 5 }, X::TypeCheck::Assignment, 'a typed attribute container refuses a wrong type';
$s = 'ok';
is $s, 'ok', 'and takes the right one';

my Mu $copy := nqp::clone_nd($s);
throws-like { $copy = 7 }, X::TypeCheck::Assignment, 'a clone of it keeps the type';

my Mu $any := C.^attributes[1].auto_viv_container;
$any = 5;
is $any, 5, 'an untyped attribute container takes anything';
is-deeply $copy, 'ok', 'the clone holds the value it was cloned with';
