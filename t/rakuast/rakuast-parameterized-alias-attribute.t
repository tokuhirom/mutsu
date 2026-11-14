use Test;
use NativeCall;

# An attribute typed by a lowercase constant alias of a parametric type,
# parameterized in place (`has gpointer[T] $.data`). Its implicit default is the
# bareword `gpointer[T]`, which the RakuAST frontend refused as an unsupported
# construct, so the whole compilation unit failed to load. Reduced from
# Gnome::Cairo 0.3.0's Types.rakumod (`constant \gpointer = Pointer` comes from
# Gnome::N::GlibToRakuTypes), which blocked every Gnome::Gtk3 module.

plan 7;

constant \gpointer = Pointer;
constant arr = Array;

class T is repr('CStruct') { has int32 $.x }
class S is repr('CStruct') { has gpointer[T] $.data }
class C {
    has arr[Int] $.a;
    has gpointer[int32] $.p;
}

is S.new.data.^name, 'NativeCall::Types::Pointer[T]',
    'a CStruct attribute typed gpointer[T] defaults to Pointer[T]';
is C.new.a.^name, 'Array[Int]', 'arr[Int] attribute defaults to Array[Int]';
is C.new.p.^name, 'NativeCall::Types::Pointer[int32]',
    'gpointer[int32] attribute defaults to Pointer[int32]';
is C.^attributes[0].type.^name, 'Array[Int]',
    'the attribute type is the parameterized aliased type';

is EVAL(Q[constant gptr = Array; class D { has gptr[Str] $.s }; D.new.s.^name]),
    'Array[Str]', 'EVAL of a string declaring such an attribute';

my $ast = Q[constant gptr = Array; class E { has gptr[Str] $.s }; E.new.s.^name].AST;
my $attr = $ast.statements[1].expression.body.body.statement-list.statements[0].expression;
isa-ok $attr.type, RakuAST::Type::Parameterized, '.AST renders the type as Type::Parameterized';
is EVAL($ast), 'Array[Str]', 'EVAL of the .AST round trip';
