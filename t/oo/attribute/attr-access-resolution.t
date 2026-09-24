use Test;

# Pins how a `$!x` / `$.x` access inside a method finds its storage: the
# running method's owner class, whether that owner is a role, and `self` are
# decided per method frame rather than per access (ADR-0121 D1). Each case
# below is one of the shapes that resolution has to keep telling apart.

plan 14;

# A private attribute declared in both a parent and a child: each class's
# methods see their own.
class Parent {
    has $!priv = 'parent';
    method parent-priv { $!priv }
    method set-parent-priv($v) { $!priv = $v }
}
class Child is Parent {
    has $!priv = 'child';
    method child-priv { $!priv }
    method set-child-priv($v) { $!priv = $v }
}
{
    my $c = Child.new;
    is $c.parent-priv, 'parent', 'parent method reads the parent private attribute';
    is $c.child-priv, 'child', 'child method reads the child private attribute';
    $c.set-parent-priv('P2');
    $c.set-child-priv('C2');
    is $c.parent-priv, 'P2', 'parent method writes its own private attribute';
    is $c.child-priv, 'C2', 'child method writes its own private attribute';
}

# A scalar and a hash attribute sharing a bare name in one class.
class Collide {
    has $!value = 1;
    has %!value = a => 2;
    method scalar { $!value }
    method hash-a { %!value<a> }
    method bump { $!value = $!value + 10 }
}
{
    my $o = Collide.new;
    $o.bump;
    is $o.scalar, 11, 'scalar of a sigil-colliding pair is read and written';
    is $o.hash-a, 2, 'hash of a sigil-colliding pair is untouched by the scalar write';
}

# A role composed at runtime: its methods reach the role's own attribute
# and fall through to the wrapped instance's attributes.
role Counted {
    has $!count = 0;
    method tick { $!count = $!count + 1; $!count }
}
class Plain {
    has $.name = 'plain';
    method shout { $!name.uc }
}
{
    my $o = Plain.new;
    $o does Counted;
    $o.tick;
    is $o.tick, 2, 'a mixin role method reads and writes its own attribute';
    is $o.shout, 'PLAIN', 'the wrapped class method still reads its attribute';
}

# A `$!x` inside a closure created in a method resolves `self` from the
# closure's captured scope, not from a local slot of its own.
class Closer {
    has $!n = 5;
    method adder { -> $k { $!n = $!n + $k; $!n } }
}
{
    my $f = Closer.new.adder;
    $f(1);
    is $f(2), 8, 'an attribute read and written from a closure inside a method';
}

# A mutation made in a nested method frame is visible to the caller.
class Nest {
    has $!v = 0;
    method inner { $!v = $!v + 1 }
    method outer { self.inner; self.inner; $!v }
}
is Nest.new.outer, 2, 'a nested frame write is seen by the calling method';

# A typed attribute still type-checks on assignment, from a subclass's
# inherited method too.
class Typed {
    has Int $!n = 0;
    method set($v) { $!n = $v }
    method n { $!n }
}
class TypedChild is Typed { }
{
    my $t = TypedChild.new;
    $t.set(3);
    is $t.n, 3, 'an inherited method writes a typed attribute';
    throws-like { $t.set('x') }, X::TypeCheck::Assignment,
        'an inherited method still type-checks a typed attribute';
}

# The same method body run on two different invocants reads each one's
# attributes.
class Two {
    has $.x;
    method twice { $!x * 2 }
}
is Two.new(x => 3).twice, 6, 'first invocant';
is Two.new(x => 7).twice, 14, 'second invocant of the same method';
