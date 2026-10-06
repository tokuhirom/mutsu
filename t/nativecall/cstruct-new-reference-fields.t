use Test;
use NativeCall;

# Reference-typed fields of a Raku-built CStruct (`Str`, a nested struct, a
# `CArray[T]`) hold pointers into memory the struct must keep alive: the struct
# retains what it was given (ADR-11209), and a `Str` field points at a copy the
# struct owns.

plan 17;

class Inner is repr<CStruct> { has int32 $.x; has int32 $.y }
class Outer is repr<CStruct> {
    has int32 $.n;
    has Str $.s;
    has Inner $.inner;
    has CArray[int32] $.arr;
    submethod BUILD(:$n, Str :$s, Inner :$inner, CArray[int32] :$arr) {
        $!n = $n if $n.defined;
        $!s := $s if $s.defined;
        $!inner := $inner if $inner.defined;
        $!arr := $arr if $arr.defined;
    }
    method noop { 1 }
}

# The children are temporaries: nothing but the struct refers to them.
sub make {
    Outer.new(n => 3, s => "hel" ~ "lo", inner => Inner.new(x => 5, y => 6),
              arr => CArray[int32].new(7, 8, 9))
}
my $o = make;
$o.noop;   # a method call refreshes the field cache from C memory
# Churn the allocator so a freed child would be overwritten.
my @churn = (1..20000).map({ Inner.new(x => 999, y => 998) });
my @strs = (1..20000).map({ "garbage-$_" ~ "x" x 20 });

is $o.n, 3, 'a native field next to them';
is $o.s, 'hello', 'a Str field reads back after its source went away';
is-deeply ($o.inner.x, $o.inner.y), (5, 6), 'a nested struct field survives';
is-deeply ($o.arr[0], $o.arr[1], $o.arr[2]), (7, 8, 9), 'a CArray field survives';

# A field reads back as the object it was given.
my $i = Inner.new(x => 1, y => 2);
my $p = Outer.new(inner => $i);
ok $p.inner === $i, 'a struct field answers the very object it was bound to';
is $o.arr.^name, 'NativeCall::Types::CArray[int32]', 'a CArray field keeps its element type';

# Unset reference fields are NULL.
my $e = Outer.new;
nok $e.inner.defined, 'an unset struct field is a type object';
nok $e.s.defined, 'an unset Str field is a type object';
nok $e.arr.defined, 'an unset CArray field is a type object';

# C sees the pointers.
sub memcpy(Outer $dst, Outer $src, size_t $n --> Pointer) is native('c', v6) { * }
my $copy = Outer.new;
memcpy($copy, $o, nativesizeof(Outer));
is-deeply ($copy.n, $copy.s), (3, 'hello'), 'a copy made by C reads the same scalar fields';
is-deeply ($copy.inner.x, $copy.arr[1]), (5, 8), 'and the same pointed-at memory';

# Rebinding a field in a method replaces what it points at.
class Named is repr<CStruct> {
    has Str $.name;
    has Inner $.inner;
    submethod BUILD(Str :$name) { $!name := $name if $name.defined }
    method rename(Str $n) { $!name := $n; self }
    method reinner(Inner $i) { $!inner := $i; self }
}
sub slen(Str --> size_t) is native('c', v6) is symbol('strlen') { * }
my $nm = Named.new(name => "first");
is $nm.name, 'first', 'a Str field bound in BUILD';
$nm.rename("second" ~ "!");
is $nm.name, 'second!', 'a Str field rebound in a method';
is slen($nm.name), 7, 'and that is what C reads';
@strs = (1..20000).map({ "more-garbage-$_" ~ "y" x 20 });
is $nm.name, 'second!', 'the replacement survives allocator churn';
$nm.reinner(Inner.new(x => 33));
@churn = (1..20000).map({ Inner.new(x => 1, y => 1) });
is $nm.inner.x, 33, 'a struct field rebound in a method survives';
nok Named.new.inner.defined, 'and an unbound one is a type object';
