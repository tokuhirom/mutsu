use Test;

# `my $x := EXPR` bound to a value with no container of its own takes the
# env-free bind-declaration lane (`exec_set_local_bind_decl_fast`). The
# binding must stay readonly, rebindable, and invisible to every other
# binding of the name, exactly as the general store left it.

plan 14;

class C {
    has $!x = 1;
    method foo { 3 }
    method readonly-after-bind {
        my $v := self.foo;
        try { $v = 5 };
        ($v, $!.^name, $!.message)
    }
    method rebind { my $v := self.foo; $v := 9; $v }
    method incr { my $v := "s"; try { $v++ }; $!.^name }
    method object { my $v := self; $v.^name }
    method code { my $v := -> { 41 + 1 }; $v() }
    method attr { my $v := $!x; $v = 7; $!x }
    method array { my @a = 1, 2; my $v := @a; $v.push(3); @a.join(',') }
    method list { my $v := (1, 2, 3); $v.elems }
    method nil { my $v := Nil; $v.raku }
    method loop { my @out; for ^3 { my $v := $_ * 2; @out.push($v) }; @out.join(',') }
}

my $c = C.new;
is-deeply $c.readonly-after-bind,
    (3, 'X::AdHoc', 'Cannot assign to an immutable value'),
    'a bind to a call result is readonly';
is $c.rebind, 9, 'a value-bound name can be rebound';
is $c.incr, 'X::Multi::NoMatch', '++ on a value-bound name refuses';
is $c.object, 'C', 'a bind to an object';
is $c.code, 42, 'a bind to a code value';
is $c.attr, 7, 'a bind to an attribute container still writes through';
is $c.array, '1,2,3', 'a bind to an array aliases it';
is $c.list, 3, 'a bind to a list';
is $c.nil, 'Nil', 'a bind to Nil';
is $c.loop, '0,2,4', 'a fresh bind per loop iteration';

sub caller-x { my $x := 10; callee-x() + $x }
sub callee-x { my $x = 1; $x = 2; $x }
is caller-x(), 12, "a callee's own same-named \$x is not readonly";

sub twice { my $r := 5; try { $r = 6 }; $r }
is twice(), 5, 'first call keeps the value';
is twice(), 5, 'second call starts from a fresh binding';

sub type-object { my $t := Int; try { $t = 5 }; $!.message }
is type-object(), 'assign requires a concrete object (got a Int type object instead)',
    'a bind to a type object names the type in its error';
