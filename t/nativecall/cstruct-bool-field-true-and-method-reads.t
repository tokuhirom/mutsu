use NativeCall;
use Test;

# Found via CSS::Box over CSS::Properties (#14353): CSS::Module::Property is an
# `is repr('CStruct')` class whose `method box { ? $!box }` reads the field.
# 1. a `bool` field given `True` stored a zero byte;
# 2. a method that is eligible for the compiled fast path read a stale cell
#    instead of the C memory, so `$obj.field = v` was invisible to `$!field`.

plan 8;

class Flags is repr('CStruct') {
    has bool $.box;
    has uint16 $.n;
    submethod BUILD(:$!box = 0, :$!n = 0) { }
    method boxed { ? $!box }
}

is Flags.new(:box).box, 1, 'named True reaches the bool field';
is Flags.new(:box(False)).box, 0, 'named False';
is Flags.new(:box, :n(7)).boxed, True, 'a method reading $!box sees True';
is Flags.new(:n(7)).boxed, False, 'default is false';

class Rect is rw is repr('CStruct') {
    has num32 $.top;
    has num32 $.left = 0e0;
    method raw { $!top }
    method both { ($!top, $!left) }
    method enclose(::?CLASS:D: $d) {
        my $o = self.new: :$!top, :$!left;
        $o.top += $d;
        $o;
    }
}

my $r = Rect.new(:top(80e0));
$r.top = 7e0;
is $r.raw, 7, 'a method sees an accessor assignment';
is $r.both.join(','), '7,0', 'a list-returning method too';
$r.top += 5;
is $r.raw, 12, 'compound assignment through the accessor';
is Rect.new(:top(80e0)).enclose(5).both.join(','), '85,0', 'enclose-style clone then +=';
