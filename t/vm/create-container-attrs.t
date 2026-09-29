use Test;

# From ecosystem dist String::Fields: `self.CREATE!SET-SELF(...)` pushes onto
# native-typed array attributes that CREATE must seed as empty containers.
plan 4;

class B {
    has int @!f;
    has Str @!s;
    has %!h;
    method new() { self.CREATE!SET }
    method !SET { @!f.push(0); @!f.push(7); @!s.push("a"); %!h<a> = 1; self }
    method f { @!f }
    method s { @!s }
    method h { %!h }
}
my $b = B.new;
is $b.f.join(","), "0,7", 'native int array attribute is an empty array after CREATE';
is $b.s.join(","), "a", 'typed array attribute';
is-deeply $b.h, {a => 1}, 'hash attribute';
is B.new.f.elems, 2, 'fresh instance starts empty';
