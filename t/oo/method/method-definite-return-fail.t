# A `fail` inside a method hands back its Failure even when the signature pins the return value
# (`--> True`, `--> 42`): the pinned value is what a method that COMPLETES yields (#17077).
use Test;
plan 9;

class C {
    method a(Str $s --> True) { fail "no $s" }
    method b(Str $s --> True) { 0 || fail "no $s" }
    method d(Str $s --> Int)  { fail "no $s" }
    method e(Str $s --> 42)   { fail "no $s" }
    method ok(Str $s --> True) { 1 }
    method win(Str $s --> 42) { 7 }
    method thrower(Str $s --> True) { X::AdHoc.new(payload => "x").throw }
    method guarded(Str $s --> True) { $s eq 'bad' || fail X::AdHoc.new(payload => "cant $s") }
}

isa-ok C.new.a("z"), Failure, '`fail` with --> True';
isa-ok C.new.b("z"), Failure, '`||` fail with --> True';
isa-ok C.new.d("z"), Failure, '--> Int still passes a Failure through';
isa-ok C.new.e("z"), Failure, '`fail` with --> 42';
is C.new.ok("z"), True, 'a completing method yields the pinned True';
is C.new.win("z"), 42, 'a completing method yields the pinned 42';
dies-ok { C.new.thrower("z") }, 'a throw still propagates';
isa-ok C.new.guarded("good"), Failure, 'the guard idiom (Math::FFT::Libfftw3 plan-save) returns the Failure';
throws-like { C.new.guarded("good").Str }, X::AdHoc, 'and using it throws the typed exception';
