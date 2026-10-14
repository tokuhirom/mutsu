# A definite (`Foo:D`, `Foo:U`) or coercion (`Int(Str)`) type object is
# `Uninstantiable`, as DefiniteHOW/CoercionHOW make it in rakudo. NativeCall's
# `check_routine_sanity` warned about a `--> ::?CLASS:D` return of a CStruct
# class (FontConfig's `FcFontSet::create`) because it read `P6opaque`.
use Test;
use NativeCall;
use NativeCall :TEST;

plan 9;

class Pt is repr('CStruct') {
    has int32 $.x;
    method d(--> ::?CLASS:D) { }
}

is Int:D.REPR, 'Uninstantiable', 'Int:D';
is Int:U.REPR, 'Uninstantiable', 'Int:U';
is Int:_.REPR, 'P6opaque', 'Int:_ is Int itself';
is Int(Str).REPR, 'Uninstantiable', 'coercion type';
is Int().REPR, 'Uninstantiable', 'coercion type without a source';
is Int.REPR, 'P6opaque', 'plain type';
is Pt.REPR, 'CStruct', 'CStruct class';
is Pt.^find_method('d').returns.REPR, 'Uninstantiable', '::?CLASS:D return type';

sub make-pt(--> Pt:D) { }
my $warned = False;
{
    CONTROL { when CX::Warn { $warned = True; .resume } }
    check_routine_sanity(&make-pt);
}
nok $warned, 'no NativeCall sanity warning for a definite CStruct return';
