use Test;
use NativeCall;

# `.REPR` and `.WHERE` for a NativeCall handle — an object whose whole identity
# is a C address. Together they are what `MoarVM::Guts::REPRs`' `BODY_OF` needs:
# it dispatches on `.REPR` and then *dereferences* `.WHERE`, so an honest
# `.REPR` is a promise that a REPR body sits at `.WHERE`.
#
# mutsu's `.WHERE` contract is "points straight at the payload, no object
# header", so the probe in `MoarVM::Guts::REPRs` computes `Offset` as 0 and the
# body is read at `.WHERE` itself. The bodies below are the same hand-written
# CStruct mirrors that module declares.

plan 15;

sub calloc(size_t, size_t --> Pointer) is native { * }
sub free(Pointer) is native { * }

class CStructB is repr('CStruct') {
    has Pointer          $.cstruct;
    has Pointer[Pointer] $.child_objs;
}
class CArrayB is repr('CStruct') {
    has Pointer          $.storage;
    has Pointer[Pointer] $.child;
    has int32            $.managed;
    has int32            $.allocated;
    has int32            $.elems;
}

class Rec is repr('CStruct') {
    has int64 $.one is rw;
    has int64 $.two is rw;
}

# How far into an object its body sits is implementation-defined — Rakudo puts
# it past MoarVM's object header, mutsu's `.WHERE` points straight at it — so
# scan for it the way `MoarVM::Guts::REPRs` does rather than assuming an offset.
sub body-offset($where, $wanted) {
    my $words = nativecast(CArray[uint64], Pointer.new($where));
    my $i = 0;
    repeat { last if $words[$i] == $wanted } while ++$i < 10;
    $i == 10 ?? Nil !! $i * 8
}

# --- a CStruct handle ---
my $blk = calloc(1, 16);
ok $blk.defined, 'calloc gave us a block to work in';

my $r = nativecast(Rec, $blk);
is $r.REPR, 'CStruct',            'a nativecast CStruct handle reports CStruct';
isnt $r.WHERE, 0,                 'and has a non-zero WHERE';

# What BODY_OF does, spelled out.
my $off = body-offset($r.WHERE, $blk.Int);
ok $off.defined,                  'the CStruct body is found within ten words';
my $body = nativecast(CStructB, Pointer.new($r.WHERE + $off));
is $body.cstruct.Int, $blk.Int,   'the CStruct body points at the struct itself';

$r.one = 5;
is nativecast(Rec, $body.cstruct).one, 5,
                                  'and going back through it reaches the same memory';

# --- a CArray handle ---
my $ca = nativecast(CArray[int32], $blk);
is $ca.REPR, 'CArray',            'a nativecast CArray handle reports CArray';
my $coff = body-offset($ca.WHERE, $blk.Int);
ok $coff.defined,                 'the CArray body is found within ten words';
my $cb = nativecast(CArrayB, Pointer.new($ca.WHERE + $coff));
is $cb.storage.Int, $blk.Int,     'the CArray body points at the storage';
is $cb.managed, 0,                'a cast CArray is not managed';
is $cb.elems, 0,                  'and carries no element count';

free($blk);

# --- what deliberately does NOT get a body ---
# The three assertions below pin a *deliberate under-report*, so they are the
# one part of this file that does not match raku: raku answers `CStruct` and
# `VMArray` here, because those objects have C storage and mutsu's do not yet.
# Reporting the honest name without a body is the one thing that must never
# happen — `BODY_OF` would dereference whatever `.WHERE` returned. Under-report
# and it refuses loudly instead. Giving these objects real storage (and with it
# the honest name) is ADR-0015's P2/P3.
is Rec.new.REPR, 'P6opaque',      'a Raku-constructed CStruct has no body yet';

class Plain { has $.address = 12345; }
is Plain.new.REPR, 'P6opaque',    'an ordinary class is untouched';
isnt Plain.new.WHERE, Plain.new.WHERE,
                                  'and keeps identity-derived WHERE';
is Buf.new(1, 2, 3).REPR, 'P6opaque',
                                  'a Buf has no body yet either';
