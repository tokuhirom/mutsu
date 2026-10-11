use Test;
use nqp;

# nqp::box_{s,n,i} into a class that inherits the matching native scalar
# builds an instance of that class carrying the value (#12704). ValueMap's
# `WHICH` boxes into the core `ValueObjAt` (an `is Str` class).

plan 22;

my $o := nqp::box_s("abc", ValueObjAt);
isa-ok $o, ValueObjAt, 'box_s into ValueObjAt';
is ~$o, 'abc', 'the ValueObjAt carries the string';
is nqp::unbox_s($o), 'abc', 'unbox_s reads it back';
isa-ok nqp::box_s("w", ObjAt), ObjAt, 'box_s into ObjAt';

my class Holder {
    has ValueObjAt $!which;
    method which { $!which := nqp::box_s("x", ValueObjAt); $!which }
}
isa-ok Holder.new.which, ValueObjAt, 'binds into a ValueObjAt-typed attribute';

my class MyStr is Str { has $.tag = 't'; method hi { "hi " ~ self } }
my $s := nqp::box_s("q", MyStr);
isa-ok $s, MyStr, 'box_s into an is Str subclass';
is $s.hi, 'hi q', 'subclass method sees the payload';
is $s.chars, 1, 'Str method runs on the payload';
ok $s ~~ Str, 'still a Str';
is $s.raku, '"q"', '.raku';
is nqp::unbox_s($s), 'q', 'unbox_s';

my class MyNum is Num { }
my $n := nqp::box_n(1.5e0, MyNum);
isa-ok $n, MyNum, 'box_n into an is Num subclass';
is $n + 1, 2.5, 'arithmetic on the payload';
is nqp::unbox_n($n), 1.5e0, 'unbox_n';

my class MyInt is Int { }
my $i := nqp::box_i(5, MyInt);
isa-ok $i, MyInt, 'box_i into an is Int subclass';
is $i + 1, 6, 'arithmetic on the payload';
is nqp::unbox_i($i), 5, 'unbox_i';
isa-ok nqp::box_i(5, MyInt.new(3)), MyInt, 'an instance type operand boxes into its class';

isa-ok nqp::box_s("z", Str), Str, 'box_s into Str is a plain Str';
nok nqp::box_s("z", Str) ~~ MyStr, '... not a subclass';

throws-like { nqp::box_s("z", MyInt) }, Exception,
    message => 'This type cannot box a native string: P6opaque, MyInt',
    'box_s into an is Int subclass dies';
throws-like { nqp::box_i(1, MyStr) }, Exception,
    message => 'This type cannot box a native integer: P6opaque, MyStr',
    'box_i into an is Str subclass dies';
