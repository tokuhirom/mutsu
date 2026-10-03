use v6;
use Test;
use lib 't/lib';
use CodeVarFreeReadUnit;

# A routine's free `&name` is the binding visible where the routine was
# declared, never a same-named `my &name` of whoever calls it. Reduced from
# URI::Template, whose class-body `my sub uri-encode` passes `&enc` to
# `.subst` while the calling method has a `my &enc` of its own.

plan 9;

my &outer = sub ($x) { "outer" };
sub read-value() { my $f = &outer; $f("x") }
sub read-call()  { &outer("x") }
sub caller-of-mainline() {
    my &outer = sub ($x) { "caller" };
    (read-value(), read-call())
}
is-deeply caller-of-mainline(), ("outer", "outer"),
    'mainline sub reads its own &outer, not the caller\'s';

{
    my &blk = sub ($x) { "block" };
    sub blk-read() { my $f = &blk; $f("x") }
    sub blk-caller() { my &blk = sub ($x) { "caller" }; blk-read() }
    is blk-caller(), "block", 'block-scoped sub reads its block\'s &blk';
}

class Enc {
    my &enc = sub ($m) { "<" ~ $m.Str ~ ">" };
    my sub encode(Str:D $text) { $text.subst(/b/, &enc, :g) }
    my sub bare(Str:D $text) { enc($text) }
    method value-read() { my &enc = &encode; "abc".map(&enc).join }
    method bare-call()  { my &enc = sub ($m) { "caller" }; bare("b") }
    method own-local()  { my &enc = sub ($m) { "own" }; enc("x") ~ &enc("y") }
}
is Enc.value-read, "a<b>c", 'class-body sub passes the class body\'s &enc';
is Enc.bare-call, "<b>", 'class-body sub bare-calls the class body\'s &enc';
is Enc.own-local, "ownown", 'a method\'s own my &enc still shadows the class body\'s';

sub unit-caller() {
    my &enc = sub ($m) { "caller" };
    (unit-value(), unit-call())
}
is-deeply unit-caller(), ("unit", "unit"),
    'module routine reads the module\'s file-scope &enc';

sub takes-param(&outer) { &outer("x") ~ outer("y") }
is takes-param(sub ($x) { "param" }), "paramparam",
    'a &-parameter shadows the mainline &outer';

my &later;
sub read-later() { &later("x") }
&later = sub ($x) { "assigned" };
is read-later(), "assigned", 'a reassigned mainline &later is read live';

sub nested() {
    my &n = sub ($x) { "outer-n" };
    { my &n = sub ($x) { "inner-n" }; return &n("x") }
}
is nested(), "inner-n", 'an inner block\'s my &n shadows the outer one';
