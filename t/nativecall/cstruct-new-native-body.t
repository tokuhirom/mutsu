use Test;
use NativeCall;

# A CStruct built in Raku (`.new`, `.bless`, `nqp::create`) owns a zeroed block
# of C memory laid out like the struct, and that block -- not Raku-side
# attribute storage -- is the object's state (ADR-11209). It used to be an
# ordinary instance with no C storage: it reported `P6opaque`, could not be
# cast, and reached C as NULL (a SIGSEGV through `memcpy`).

plan 34;

class Rec is repr<CStruct> {
    has int32 $.a;
    has num64 $.d;
    has int8  $.c;
    has int32 $.dflt = 7;
    method bump { $!a = $!a + 1; self }
    method seta($v) { $!a = $v }
}

my $t = Rec.new(a => 1, d => 2.5e0, c => 3);
is $t.REPR, 'CStruct', 'a Raku-built CStruct reports CStruct';
is Rec.REPR, 'CStruct', 'and so does its type object';
is-deeply ($t.a, $t.d, $t.c), (1, 2.5e0, 3), 'named arguments reach the fields';
is $t.dflt, 7, 'a declared default reaches the field';
is nativesizeof(Rec), 24, 'the struct has its C layout';

# A pointer to the body exists and is what `.WHERE`'s family and C see.
my $p = nativecast(Pointer, $t);
ok $p.defined, 'nativecast(Pointer, $struct) is a Pointer';
isnt $p.Int, 0, 'to a non-NULL block';
is $p.Int % 8, 0, 'aligned for the widest field';

# Methods read and write the body.
$t.bump;
is $t.a, 2, '$!a = $!a + 1 in a method updates the field';
$t.seta(40);
is $t.a, 40, 'a method assignment is visible through the accessor';
is nativecast(CArray[int32], $p)[0], 40, 'and in the C memory';

# The struct reaches C as a real pointer.
sub memcpy(Rec $dst, Rec $src, size_t $n --> Pointer) is native('c', v6) { * }
my $u = Rec.new;
is-deeply ($u.a, $u.d, $u.c, $u.dflt), (0, 0e0, 0, 7), 'an empty .new is zero but for defaults';
memcpy($u, $t, nativesizeof(Rec));
is-deeply ($u.a, $u.d, $u.c, $u.dflt), (40, 2.5e0, 3, 7), 'memcpy between two Raku-built structs copies the fields';

# C writes are seen, even when Raku had set the field before.
class Timeval is repr<CStruct> { has int64 $.sec; has int64 $.usec }
sub gettimeofday(Timeval, Pointer --> int32) is native('c', v6) { * }
my $tv = Timeval.new(sec => 1, usec => 2);
is gettimeofday($tv, Pointer), 0, 'a struct Raku built is filled in by C';
ok $tv.sec > 1_000_000_000, 'the field C wrote reads back, not the one Raku set';
ok $tv.usec >= 0, 'both fields';

# is rw accessors and private attributes live in the body too.
class Mix is repr<CStruct> {
    has int64 $.one is rw;
    has int32 $.two is rw;
    has int32 $!priv;
    method priv { $!priv }
    method set-priv($v) { $!priv = $v; self }
}
my $m = Mix.new(one => 5, two => 6);
$m.one = 50;
$m.two += 1;
is-deeply ($m.one, $m.two), (50, 7), 'is rw accessors write the body';
$m.set-priv(9);
is $m.priv, 9, 'a private attribute round-trips';
is $m.raku, 'Mix.new(one => 50, two => 7)', '.raku shows the live fields';
is $m.gist, 'Mix.new(one => 50, two => 7)', '.gist shows the live fields';

# BUILD and TWEAK assignments are migrated into the body.
class Cfg is repr<CStruct> {
    has int32 $.w;
    has int32 $.h;
    has int32 $.area;
    submethod BUILD(:$!w = 3, :$!h = 4) { $!area = $!w * $!h }
}
my $c = Cfg.new(w => 5);
is-deeply ($c.w, $c.h, $c.area), (5, 4, 20), 'a BUILD that assigns is migrated';
class Tweaked is repr<CStruct> {
    has int32 $.w;
    has int32 $.sq;
    submethod TWEAK { $!sq = $!w * $!w }
}
is Tweaked.new(w => 6).sq, 36, 'so is a TWEAK';

# bless, nqp::create, and the object's own lifetime.
is Rec.bless(a => 9).a, 9, '.bless allocates the body too';
use nqp;
my $bare = nqp::create(Rec);
is $bare.REPR, 'CStruct', 'nqp::create gives a body';
is-deeply ($bare.a, $bare.d, $bare.dflt), (0, 0e0, 0), 'zeroed, with no defaults run';
my @many = (1..2000).map({ Rec.new(a => $_) });
is @many[1234].a, 1235, 'many structs keep their own storage';
is ([+] @many.map(*.a)), 2001000, 'none aliases another';

# Inheritance extends the parent's layout.
class Base is repr<CStruct> { has int32 $.a; }
class Derived is repr<CStruct> is Base { has int32 $.b; }
my $d = Derived.new(a => 1, b => 2);
is-deeply ($d.a, $d.b, nativesizeof(Derived)), (1, 2, 8), 'a derived struct has both fields';

# HAS members live inline.
class Inner is repr<CStruct> { has int32 $.x; has int32 $.y }
class Line is repr<CStruct> { HAS Inner $.a; HAS Inner $.b; has int32 $.w; }
my $l = Line.new(w => 4);
is nativesizeof(Line), 20, 'a struct with two HAS members is 20 bytes';
is $l.w, 4, 'a field after them is where C puts it';
ok $l.a.defined, 'an inline member is a defined handle';
is $l.a.x, 0, 'onto zeroed storage';
class Mat is repr<CStruct> { HAS num32 @.m[4] is CArray; has int32 $.n; }
my $mat = Mat.new(n => 7);
$mat.m[2] = 1.5e0;
is-deeply ($mat.n, $mat.m[2], nativesizeof(Mat)), (7, 1.5e0, 20), 'an inline array lives in the body';

# A class whose objects are plain stays plain.
class Plain { has $.address = 12345; }
is Plain.new.REPR, 'P6opaque', 'an ordinary class is untouched';
