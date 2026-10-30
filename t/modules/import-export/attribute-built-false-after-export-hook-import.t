use v6;
use lib 't/lib';
use Test;
use PlainExportHook;

plan 4;

# A unit that imports through a run-time `sub EXPORT` hook keeps `False`/`True`
# shadowable (#9047); `is built(False)` is still the core trait, not an unknown
# one (#15160, MoarVM::Bytecode via `use paths`).
class C {
    has Int $.a is built(False);
    has Int $.b is built(True);
    has Int $.c is built;
}

my $o = C.new(:a(1), :b(2), :c(3));
is $o.a, Int, 'is built(False) attribute is not set by .new';
is $o.b, 2, 'is built(True) attribute is set';
is $o.c, 3, 'is built attribute is set';
is C.^attributes[0].name, '$!a', 'class composed';
