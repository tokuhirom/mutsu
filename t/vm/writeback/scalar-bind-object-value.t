use Test;
plan 56;

class BoundObject {
    has $.value is rw = 1;
}
{
    role BoundRole { method bound-answer { 7 } }
    my $routine := sub { 1 };
    my $alias := $routine;
    $routine does BoundRole;
    is $routine.bound-answer, 7, 'a value-bound routine can receive a role';
    is $alias.bound-answer, 7, 'the routine alias sees the composed role';
    is $routine.VAR.^name, $routine.^name, 'composition preserves the value binding';
    dies-ok { $routine = 5 }, 'composition does not make the value binding assignable';
}
{
    my $o := BoundObject.new;
    is $o.VAR.^name, 'BoundObject', 'an object value bind has no Scalar';
    throws-like { $o = 5 }, X::AdHoc, message => / 'Cannot assign to an immutable value' /,
        'assignment cannot replace a directly bound object';
    $o.value = 2;
    is $o.value, 2, 'the bound object retains its writable attributes';
    ok $o.self =:= $o, 'a direct object bind retains object container identity';
}
{
    my $o := BoundObject.new;
    my $reflect = 'VAR';
    {
        my $o = 5;
        is $o.VAR.^name, 'Scalar', 'a shadowing ordinary declaration owns a Scalar';
        is $o."$reflect"().^name, 'Scalar', 'a dynamic reflector uses the shadowing receiver slot';
        $o = 6;
        is $o, 6, 'the shadowing Scalar remains writable';
    }
    is $o.VAR.^name, 'BoundObject', 'leaving a shadow restores the outer binding identity';
    is $o."$reflect"().^name, 'BoundObject', 'dynamic reflection uses the restored outer binding';
    dies-ok { $o = 7 }, 'leaving a shadow retains the outer immutable object bind';
}
{
    sub object-bind() {
        my $o := BoundObject.new;
        is $o.VAR.^name, 'BoundObject', 'a routine object bind has no Scalar';
        dies-ok { $o = 8 }, 'a routine object bind is immutable';
    }
    object-bind();
    object-bind();
}
{
    my $source = BoundObject.new;
    my $alias := $source;
    is $alias.VAR.^name, 'Scalar', 'binding an object-containing Scalar shares that Scalar';
    $alias = 9;
    is $source, 9, 'assignment through a Scalar alias still writes through';
}
{
    my $stored = 1;
    my $p := Proxy.new(FETCH => -> $ { $stored }, STORE => -> $, $value { $stored = $value });
    is $p.VAR.^name, 'Proxy', 'a bound Proxy keeps its own container';
    lives-ok { $p = 10 }, 'binding a Proxy preserves its STORE';
    is $stored, 10, 'the Proxy receives the assigned value';
}
{
    my $o := BoundObject.new;
    my &writer = { $o = 11 };
    sub invoke-with-shadow($o, &write) { write() }
    dies-ok { invoke-with-shadow(0, &writer) }, 'a closure asks its object binding instead of a caller name';
    is $o.VAR.^name, 'BoundObject', 'a caller shadow leaves object identity intact';
}
{
    class ObjectOwner { has $.child = BoundObject.new }
    my $child := ObjectOwner.new.child;
    is $child.VAR.^name, 'BoundObject', 'a readonly accessor result binds the object value';
    dies-ok { $child = 12 }, 'a bound readonly accessor object cannot be replaced';
    $child.value = 13;
    is $child.value, 13, 'attributes of the accessor result remain writable';
}
{
    my $o := BoundObject.new;
    $o := BoundObject.new(value => 14);
    is $o.value, 14, 'an immutable object binding can be rebound to another object';
}

# Container identity follows the same value-binding decision for built-in
# values too; retiring the separate marker must not manufacture Scalars here.
for { sub { 42 } }, { /a/ }, { Buf.new(1,2) }, { v1.2.3 },
    { \(:a(1)) }, { 1.FatRat }, { set(1,2) },
    { Promise.kept(1) }, { Channel.new } -> &produce {
    my $value := produce();
    is $value.VAR.^name, $value.^name, 'a bare built-in value keeps its own container identity';
    dies-ok { $value = 0 }, 'a bare built-in value cannot be replaced through its bound name';
    ok $value.self =:= $value, 'a value bind compares with its own decontainerized value';
}
