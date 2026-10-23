use v6;
use Test;
use NativeCall;

# A CStruct keeps its fields in native memory, so a method of the class that
# reads `$!field` is handed the fields re-read on entry. The seeding step is a
# no-op for every other class and is skipped outright in a program that declares
# no CStruct (#14639); both sides are pinned.

plan 6;

class Plain {
    has $.v = 7;
    method double { $!v * 2 }
}
is Plain.new.double, 14, 'a plain class method reads its attribute before any CStruct exists';

class Pt is repr('CStruct') {
    has int32 $.x is rw;
    has int32 $.y is rw;
    method sum { $!x + $!y }
    method bump { $!x = $!x + 1; self }
}

my $p = Pt.new(x => 3, y => 4);
is $p.sum, 7, 'a CStruct method reads $!x and $!y from the native memory';
is $p.bump.x, 4, 'a write through $!x reaches the struct';
is $p.sum, 8, 'the next entry re-reads the changed field';

is Plain.new(v => 5).double, 10, 'a plain class still works once a CStruct exists';

class Holder { has Pt $.pt; method total { $!pt.sum } }
is Holder.new(pt => Pt.new(x => 1, y => 2)).total, 3, 'a plain class holding a CStruct';
