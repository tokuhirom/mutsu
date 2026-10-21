use Test;
use nqp;

plan 9;

# `Attribute.auto_viv_container` hands out a container whose descriptor carries
# the attribute's whole constraint: its type and its `where` clause.
class C {
    has Str $.w where *.starts-with("a");
    has $.n where * > 3;
    has Int $.plain;
}

my Mu $w := C.^attributes[0].auto_viv_container;
throws-like { $w = "zzz" }, X::TypeCheck::Assignment, "a value failing the where clause is refused";
lives-ok { $w = "abc" }, "a value satisfying it is stored";
is $w, "abc", "and read back";
throws-like { $w = 42 }, X::TypeCheck::Assignment, "the type is still checked";

my Mu $n := C.^attributes[1].auto_viv_container;
throws-like { $n = 1 }, X::TypeCheck::Assignment, "an untyped attribute's where clause is enforced";
lives-ok { $n = 9 }, "and accepts a conforming value";

my Mu $copy := nqp::clone_nd($w);
throws-like { $copy = "zzz" }, X::TypeCheck::Assignment, "a clone keeps the where clause";

my Mu $plain := C.^attributes[2].auto_viv_container;
lives-ok { $plain = 5 }, "an attribute without a where clause is unaffected";
throws-like { $plain = "x" }, X::TypeCheck::Assignment, "and keeps its type check";
